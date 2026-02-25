use std::collections::HashSet;
use std::fmt::{
    self,
    Display,
};
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering::SeqCst;

use crate::file_version::FileVersion;
use crate::paths::FileId;
use crate::utils::{
    FastDashMap,
    FastDashSet,
};

#[derive(PartialEq, Eq, Clone, Hash)]
pub enum DataType {
    Any,
    Number,
    Int,
    Boolean,
    Text,
    Null,
    Array(Box<DataType>),
    Union(Vec<DataType>),
    Generic(usize),
    Failable(Box<DataType>),
    Error,
}

impl DataType {
    pub fn to_string(&self, generics_map: &GenericsMap) -> String {
        match self {
            DataType::Any => "Any".to_string(),
            DataType::Number => "Num".to_string(),
            DataType::Int => "Int".to_string(),
            DataType::Boolean => "Bool".to_string(),
            DataType::Text => "Text".to_string(),
            DataType::Null => "Null".to_string(),
            DataType::Array(t) => format!("[{}]", t.to_string(generics_map)),
            DataType::Union(types) => {
                let mut seen = HashSet::new();
                types
                    .iter()
                    .map(|t| t.to_string(generics_map))
                    .filter(|t| seen.insert(t.clone()))
                    .collect::<Vec<String>>()
                    .join(" | ")
            }
            DataType::Generic(id) => generics_map.get(*id).to_string(generics_map),
            DataType::Failable(t) => match *t.clone() {
                DataType::Union(_) => format!("({})?", t.to_string(generics_map)),
                _ => format!("{}?", t.to_string(generics_map)),
            },
            DataType::Error => "<Invalid type>".to_string(),
        }
    }
}

impl fmt::Debug for DataType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string(&GenericsMap::new()))
    }
}

#[derive(Debug, Clone)]
pub struct GenericsMap {
    map: FastDashMap<usize, DataType>,
    inferred: FastDashSet<usize>,
    generics_per_file: FastDashMap<(FileId, FileVersion), Vec<usize>>,
}

static ATOMIC_COUNTER: AtomicUsize = AtomicUsize::new(0);

impl Default for GenericsMap {
    fn default() -> Self {
        Self::new()
    }
}

impl GenericsMap {
    pub fn new() -> Self {
        Self {
            map: FastDashMap::default(),
            inferred: FastDashSet::default(),
            generics_per_file: FastDashMap::default(),
        }
    }

    #[inline]
    pub fn new_generic_id(&self) -> usize {
        ATOMIC_COUNTER.fetch_add(1, SeqCst)
    }

    pub fn reset_counter(&self) {
        ATOMIC_COUNTER.store(0, SeqCst);
    }

    pub fn constrain_generic_type(&self, id: usize, constraint: DataType) {
        if self.has_ref_to_generic(&constraint, id) {
            return;
        }

        self.constrain(id, constraint);
    }

    fn constrain(&self, id: usize, constraint: DataType) {
        match self.get(id) {
            DataType::Generic(id) => {
                self.constrain_generic_type(id, constraint);
            }
            ty if self.is_more_or_equally_specific(&ty, &constraint) => {
                self.unify_inner_generics(&ty, &constraint);
                self.map.insert(id, constraint);
            }
            _ => {}
        }
    }

    /// When replacing a type that contains generics (e.g. `Array(Generic(inner_id))`) with
    /// a more specific type (e.g. `Array(Int)`), also constrain the nested generics so
    /// parametric return types can be resolved at call sites.
    fn unify_inner_generics(&self, current: &DataType, new: &DataType) {
        match (current, new) {
            (DataType::Array(curr_inner), DataType::Array(new_inner)) => {
                self.unify_inner_generics(curr_inner, new_inner);
            }
            (DataType::Failable(curr_inner), DataType::Failable(new_inner)) => {
                self.unify_inner_generics(curr_inner, new_inner);
            }
            (DataType::Union(curr_types), DataType::Union(new_types))
                if curr_types.len() == new_types.len() =>
            {
                for (c, n) in curr_types.iter().zip(new_types.iter()) {
                    self.unify_inner_generics(c, n);
                }
            }
            (DataType::Generic(id), new_ty) if !matches!(new_ty, DataType::Generic(_)) => {
                self.constrain_generic_type(*id, new_ty.clone());
            }
            _ => {}
        }
    }

    #[inline]
    pub fn mark_as_inferred(&self, id: usize) {
        self.inferred.insert(id);
    }

    #[inline]
    pub fn is_inferred(&self, id: usize) -> bool {
        self.inferred.contains(&id)
    }

    pub fn get(&self, id: usize) -> DataType {
        let ty = self.map.get(&id).map(|t| t.value().clone());

        ty.unwrap_or(DataType::Any)
    }

    pub fn get_recursive(&self, id: usize) -> DataType {
        match self.get(id) {
            DataType::Generic(id) => self.get_recursive(id),
            DataType::Union(types) => DataType::Union(
                types
                    .iter()
                    .map(|ty| match ty {
                        DataType::Generic(id) => self.get_recursive(*id),
                        ty => ty.clone(),
                    })
                    .collect(),
            ),
            DataType::Array(ty) => match *ty {
                DataType::Generic(id) => DataType::Array(Box::new(self.get_recursive(id))),
                ty => DataType::Array(Box::new(ty.clone())),
            },
            DataType::Failable(ty) => match *ty {
                DataType::Generic(id) => DataType::Failable(Box::new(self.get_recursive(id))),
                ty => DataType::Failable(Box::new(ty)),
            },
            ty => ty,
        }
    }

    pub fn deref_type(&self, ty: &DataType) -> DataType {
        match ty {
            DataType::Generic(id) if self.is_inferred(*id) => self.get_recursive(*id),
            DataType::Union(types) => {
                DataType::Union(types.iter().map(|ty| self.deref_type(ty)).collect())
            }
            DataType::Array(ty) => DataType::Array(Box::new(self.deref_type(ty))),
            DataType::Failable(ty) => DataType::Failable(Box::new(self.deref_type(ty))),
            ty => ty.clone(),
        }
    }

    pub fn clean(&self, file_id: FileId, file_version: FileVersion) {
        self.generics_per_file
            .get(&(file_id, file_version))
            .iter()
            .for_each(|generics| {
                let ids = generics.value();
                for id in ids {
                    self.map.remove(id);
                    self.inferred.remove(id);
                }
            });
        self.generics_per_file.remove(&(file_id, file_version));
    }

    pub fn insert(&self, file_id: FileId, file_version: FileVersion, generics: Vec<usize>) {
        self.generics_per_file
            .insert((file_id, file_version), generics);
    }

    pub fn get_generics(&self, file_id: FileId, file_version: FileVersion) -> Vec<usize> {
        self.generics_per_file
            .get(&(file_id, file_version))
            .map(|generics| generics.value().clone())
            .unwrap_or_default()
    }

    fn is_more_or_equally_specific(&self, current: &DataType, new: &DataType) -> bool {
        match (current, new) {
            (DataType::Generic(id), ty) => {
                let expected = self.get(*id);
                self.is_more_or_equally_specific(&expected, ty)
            }
            (ty, DataType::Generic(id)) => {
                let given = self.get(*id);
                self.is_more_or_equally_specific(ty, &given)
            }
            (DataType::Any, _) => true,
            (_, DataType::Any) => false,
            (DataType::Array(current), DataType::Array(new)) => {
                self.is_more_or_equally_specific(current, new)
            }
            (_, DataType::Union(new_types)) => new_types
                .iter()
                .all(|new| self.is_more_or_equally_specific(current, new)),
            (DataType::Union(current_types), new) => current_types
                .iter()
                .any(|current| self.is_more_or_equally_specific(current, new)),
            (_, DataType::Error) => false,
            (DataType::Number, DataType::Int) => true,
            (t1, t2) => *t1 == *t2,
        }
    }

    fn has_ref_to_generic(&self, ty: &DataType, id: usize) -> bool {
        match ty {
            DataType::Generic(new_id) => {
                (*new_id == id) || {
                    let ty = self.get(*new_id);
                    self.has_ref_to_generic(&ty, id)
                }
            }
            DataType::Union(types) => types.iter().any(|ty| self.has_ref_to_generic(ty, id)),
            DataType::Array(ty) => self.has_ref_to_generic(ty, id),
            _ => false,
        }
    }
}

impl Display for GenericsMap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut collection = self
            .map
            .iter()
            .map(|entry| (*entry.key(), entry.value().to_string(self)))
            .collect::<Vec<(usize, String)>>();

        collection.sort_unstable_by_key(|(id, _)| *id);

        for (_, s) in collection {
            writeln!(f, "{s}")?;
        }

        Ok(())
    }
}

pub fn make_union_type(types: Vec<DataType>) -> DataType {
    if types.is_empty() {
        return DataType::Any;
    }

    let flatten_types = flatten_types(types);

    let mut seen = HashSet::new();
    let dedup_types: Vec<DataType> = flatten_types
        .into_iter()
        .filter(|ty| seen.insert(ty.clone()))
        .collect();

    if dedup_types.len() == 1 {
        dedup_types[0].clone()
    } else {
        DataType::Union(dedup_types)
    }
}

pub fn matches_type(expected: &DataType, given: &DataType, generics_map: &GenericsMap) -> bool {
    match (expected, given) {
        (DataType::Generic(id), _) => {
            let expected = generics_map.get_recursive(*id);
            matches_type(&expected, given, generics_map)
        }
        (_, DataType::Generic(id)) => {
            let given = generics_map.get_recursive(*id);
            matches_type(expected, &given, generics_map)
        }
        (_, DataType::Union(given_types)) => given_types
            .iter()
            .all(|given| matches_type(expected, given, generics_map)),
        (DataType::Union(expected_types), given_ty) => expected_types
            .iter()
            .any(|expected_type| matches_type(expected_type, given_ty, generics_map)),
        (DataType::Array(expected_type), DataType::Array(given_type)) => {
            matches_type(expected_type, given_type, generics_map)
        }
        (DataType::Any, _) | (_, DataType::Any) => true,
        (DataType::Error, _) | (_, DataType::Error) => false,
        (expected, DataType::Failable(given)) => matches_type(expected, given, generics_map),
        (DataType::Failable(expected), given) => matches_type(expected, given, generics_map),
        (t1, t2) => *t1 == *t2,
    }
}

fn flatten_types(types: Vec<DataType>) -> Vec<DataType> {
    types
        .into_iter()
        .flat_map(|t| match t {
            DataType::Union(types) => flatten_types(types),
            t => vec![t],
        })
        .collect()
}
