use std::collections::{
    HashMap,
    HashSet,
};
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

// ---------------------------------------------------------------------------
// Pure, snapshot-able generics types for Salsa-based incremental analysis
// ---------------------------------------------------------------------------

/// An immutable snapshot of generic type constraints.
///
/// This replaces the shared-mutable `GenericsMap` in the Salsa pipeline.
/// It implements `Eq` (required by Salsa for change detection) and is
/// fully deterministic — no atomic counters, no DashMaps.
#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct GenericsSnapshot {
    pub constraints: HashMap<usize, DataType>,
    pub inferred: HashSet<usize>,
}

impl GenericsSnapshot {
    /// Create a snapshot from the current state of a shared `GenericsMap`.
    pub fn from_generics_map(map: &GenericsMap) -> Self {
        let constraints: HashMap<usize, DataType> = map
            .map
            .iter()
            .map(|entry| (*entry.key(), entry.value().clone()))
            .collect();
        let inferred: HashSet<usize> = map.inferred.iter().map(|id| *id).collect();
        Self {
            constraints,
            inferred,
        }
    }

    /// Reconstruct a shared `GenericsMap` from this snapshot.
    ///
    /// Note: the global `ATOMIC_COUNTER` is NOT modified — this is only
    /// useful for read-only queries against the existing `GenericsMap` API.
    pub fn to_generics_map(&self) -> GenericsMap {
        let gm = GenericsMap::new();
        for (id, ty) in &self.constraints {
            gm.map.insert(*id, ty.clone());
        }
        for id in &self.inferred {
            gm.inferred.insert(*id);
        }
        gm
    }
}

/// Deterministic ID allocator for generic types.
///
/// Replaces the global `AtomicUsize` in the Salsa pipeline.
/// IDs are seeded from a base offset (derived from a file path hash)
/// so that IDs from different files never collide.
#[derive(Debug, Clone)]
pub struct LocalGenericsAllocator {
    next_id: usize,
    /// Base offset ensures IDs from different files don't collide.
    base: usize,
}

impl LocalGenericsAllocator {
    /// Create a new allocator with a base offset derived from a hash.
    pub fn new(base_hash: u64) -> Self {
        // Use the hash truncated and multiplied to create a large, non-overlapping range.
        let base = ((base_hash & 0x0000_FFFF_FFFF) as usize) * 100_000;
        Self { next_id: 0, base }
    }

    /// Create a zero-offset allocator (for single-file tests).
    pub fn new_zero() -> Self {
        Self {
            next_id: 0,
            base: 0,
        }
    }

    /// Allocate the next deterministic generic ID.
    pub fn next_id(&mut self) -> usize {
        let id = self.base + self.next_id;
        self.next_id += 1;
        id
    }
}

/// A pure, owned generics map for use in Salsa queries.
///
/// Same API surface as `GenericsMap` but uses `HashMap`/`HashSet`
/// instead of `DashMap`/`DashSet`, making it:
/// - Owned (no shared references needed)
/// - Deterministic (no atomic counters)
/// - `Eq`-comparable (required by Salsa)
#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct PureGenericsMap {
    map: HashMap<usize, DataType>,
    inferred: HashSet<usize>,
}

impl PureGenericsMap {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
            inferred: HashSet::new(),
        }
    }

    pub fn from_snapshot(snapshot: &GenericsSnapshot) -> Self {
        Self {
            map: snapshot.constraints.clone(),
            inferred: snapshot.inferred.clone(),
        }
    }

    pub fn into_snapshot(self) -> GenericsSnapshot {
        GenericsSnapshot {
            constraints: self.map,
            inferred: self.inferred,
        }
    }

    pub fn constrain_generic_type(&mut self, id: usize, constraint: DataType) {
        if self.has_ref_to_generic(&constraint, id) {
            return;
        }
        self.constrain(id, constraint);
    }

    fn constrain(&mut self, id: usize, constraint: DataType) {
        match self.get(id) {
            DataType::Generic(inner_id) => {
                self.constrain_generic_type(inner_id, constraint);
            }
            ty if self.is_more_or_equally_specific(&ty, &constraint) => {
                self.unify_inner_generics(&ty.clone(), &constraint);
                self.map.insert(id, constraint);
            }
            _ => {}
        }
    }

    fn unify_inner_generics(&mut self, current: &DataType, new: &DataType) {
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
                    // We need to clone to satisfy the borrow checker since
                    // unify_inner_generics takes &mut self.
                    let c = c.clone();
                    let n = n.clone();
                    self.unify_inner_generics(&c, &n);
                }
            }
            (DataType::Generic(id), new_ty) if !matches!(new_ty, DataType::Generic(_)) => {
                let id = *id;
                let new_ty = new_ty.clone();
                self.constrain_generic_type(id, new_ty);
            }
            _ => {}
        }
    }

    #[inline]
    pub fn mark_as_inferred(&mut self, id: usize) {
        self.inferred.insert(id);
    }

    #[inline]
    pub fn is_inferred(&self, id: usize) -> bool {
        self.inferred.contains(&id)
    }

    pub fn get(&self, id: usize) -> DataType {
        self.map.get(&id).cloned().unwrap_or(DataType::Any)
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

    /// Convert to a display string (for debugging).
    pub fn to_display_string(&self, ty: &DataType) -> String {
        ty.to_string(&self.to_readonly_generics_map())
    }

    /// Create a read-only `GenericsMap` for display purposes.
    fn to_readonly_generics_map(&self) -> GenericsMap {
        let gm = GenericsMap::new();
        for (id, ty) in &self.map {
            gm.map.insert(*id, ty.clone());
        }
        for id in &self.inferred {
            gm.inferred.insert(*id);
        }
        gm
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

/// `matches_type` variant that works with `PureGenericsMap`.
pub fn matches_type_pure(
    expected: &DataType,
    given: &DataType,
    generics_map: &PureGenericsMap,
) -> bool {
    match (expected, given) {
        (DataType::Generic(id), _) => {
            let expected = generics_map.get_recursive(*id);
            matches_type_pure(&expected, given, generics_map)
        }
        (_, DataType::Generic(id)) => {
            let given = generics_map.get_recursive(*id);
            matches_type_pure(expected, &given, generics_map)
        }
        (_, DataType::Union(given_types)) => given_types
            .iter()
            .all(|given| matches_type_pure(expected, given, generics_map)),
        (DataType::Union(expected_types), given_ty) => expected_types
            .iter()
            .any(|expected_type| matches_type_pure(expected_type, given_ty, generics_map)),
        (DataType::Array(expected_type), DataType::Array(given_type)) => {
            matches_type_pure(expected_type, given_type, generics_map)
        }
        (DataType::Any, _) | (_, DataType::Any) => true,
        (DataType::Error, _) | (_, DataType::Error) => false,
        (expected, DataType::Failable(given)) => matches_type_pure(expected, given, generics_map),
        (DataType::Failable(expected), given) => matches_type_pure(expected, given, generics_map),
        (DataType::Number, DataType::Int) => true,
        (t1, t2) => *t1 == *t2,
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
