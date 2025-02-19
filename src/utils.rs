use std::hash::BuildHasherDefault;

use dashmap::{DashMap, DashSet};
use rustc_hash::FxHasher;

pub type FastDashMap<K, V> = DashMap<K, V, BuildHasherDefault<FxHasher>>;
pub type FastDashSet<V> = DashSet<V, BuildHasherDefault<FxHasher>>;
