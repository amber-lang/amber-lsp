use std::collections::HashSet;

use dashmap::{DashMap, DashSet};

use crate::{grammar::alpha034::DataType, paths::FileId};

#[derive(Debug)]
pub struct GenericsMap {
    map: DashMap<(FileId, usize), DataType>,
    inferred: DashSet<(FileId, usize)>,
}

impl GenericsMap {
    pub fn new() -> Self {
        Self {
            map: DashMap::new(),
            inferred: DashSet::new(),
        }
    }

    pub fn new_generic_id(&self) -> usize {
        return self.map.len();
    }

    pub fn constrain_generic_type(&self, key: (FileId, usize), constraint: DataType) {
        if self.has_ref_to_generic(&constraint, key.0, key.1) {
            return;
        }

        self.constrain(key, constraint);
    }

    fn constrain(&self, key: (FileId, usize), constraint: DataType) {
        match self.get(key.0, key.1) {
            DataType::Generic(file_id, id) => {
                self.constrain_generic_type((file_id, id), constraint);
            }
            ty if self.is_more_or_equally_specific(&ty, &constraint) => {
                self.map.insert(key, constraint);
            }
            _ => {}
        }
    }

    pub fn mark_as_inferred(&self, key: (FileId, usize)) {
        self.inferred.insert(key);
    }

    pub fn is_inferred(&self, key: (FileId, usize)) -> bool {
        self.inferred.contains(&key)
    }

    pub fn get(&self, file_id: FileId, id: usize) -> DataType {
        let ty = self.map.get(&(file_id, id)).map(|t| t.value().clone());

        ty.unwrap_or(DataType::Any)
    }

    pub fn get_recursive(&self, file_id: FileId, id: usize) -> DataType {
        match self.get(file_id, id) {
            DataType::Generic(file_id, id) => self.get_recursive(file_id, id),
            DataType::Union(types) => DataType::Union(
                types
                    .iter()
                    .map(|ty| match ty {
                        DataType::Generic(file_id, id) => self.get_recursive(*file_id, *id),
                        ty => ty.clone(),
                    })
                    .collect(),
            ),
            DataType::Array(ty) => match *ty {
                DataType::Generic(file_id, id) => {
                    DataType::Array(Box::new(self.get_recursive(file_id, id)))
                }
                ty => DataType::Array(Box::new(ty.clone())),
            },
            ty => ty,
        }
    }

    pub fn deref_type(&self, ty: &DataType) -> DataType {
        match ty {
            DataType::Generic(file_id, id) if self.is_inferred((file_id.clone(), id.clone())) => {
                self.get_recursive(*file_id, *id)
            }
            DataType::Union(types) => {
                DataType::Union(types.iter().map(|ty| self.deref_type(ty)).collect())
            }
            DataType::Array(ty) => DataType::Array(Box::new(self.deref_type(ty))),
            ty => ty.clone(),
        }
    }

    pub fn clean(&self, file_id: &FileId) {
        self.map.retain(|k, _| k.0 != *file_id);
    }

    pub fn clone(&self) -> Self {
        let map = self.map.clone();
        let inferred = self.inferred.clone();
        Self { map, inferred }
    }

    fn is_more_or_equally_specific(&self, current: &DataType, new: &DataType) -> bool {
        match (current, new) {
            (DataType::Generic(file_id, id), ty) => {
                let expected = self.get(*file_id, *id);
                self.is_more_or_equally_specific(&expected, ty)
            }
            (ty, DataType::Generic(file_id, id)) => {
                let given = self.get(*file_id, *id);
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
            (t1, t2) => *t1 == *t2,
        }
    }

    fn has_ref_to_generic(&self, ty: &DataType, file_id: FileId, id: usize) -> bool {
        match ty {
            DataType::Generic(new_file_id, new_id) => {
                (*new_file_id == file_id && *new_id == id) || {
                    let ty = self.get(*new_file_id, *new_id);
                    self.has_ref_to_generic(&ty, file_id, id)
                }
            }
            DataType::Union(types) => types
                .iter()
                .any(|ty| self.has_ref_to_generic(ty, file_id, id)),
            DataType::Array(ty) => self.has_ref_to_generic(ty, file_id, id),
            _ => false,
        }
    }

    pub fn to_string(&self) -> String {
        let mut collection = self.map
            .iter()
            .map(|entry| {
                let (file_id, id) = entry.key();
                (*id, format!("({:?}, {}): {}", file_id, id, entry.value().to_string(self)))
            })
            .collect::<Vec<(usize, String)>>();

        collection.sort_unstable_by_key(|(id, _)| *id);
        
        collection
            .into_iter()
            .map(|(_, s)| s)
            .collect::<Vec<String>>()
            .join("\n")
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
        (DataType::Generic(file_id, id), _) => {
            let expected = generics_map.get_recursive(*file_id, *id);
            matches_type(&expected, given, generics_map)
        }
        (_, DataType::Generic(file_id, id)) => {
            let given = generics_map.get_recursive(*file_id, *id);
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
