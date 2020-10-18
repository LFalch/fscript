#[derive(Debug, Clone)]
pub struct StackTable<K, V, const N: usize> {
    inner: [(K, V); N]
}

impl<K: Ord, V, const N: usize> StackTable<K, V, N> {
    #[inline]
    pub unsafe fn new(inner: [(K, V); N]) -> Self {
        StackTable {
            inner
        }
    }
    #[inline]
    pub unsafe fn get_inner_mut(&mut self) -> &mut [(K, V)] {
        &mut self.inner[..]
    }
    #[inline]
    fn search(&self, key: &K) -> Result<usize, usize> {
        self.inner.binary_search_by_key(&key, |(k, _)| k)
    }
    #[inline]
    pub fn contains_key(&self, key: &K) -> bool {
        self.search(key).is_ok()
    }
    pub fn get(&self, key: &K) -> Option<&V> {
        if let Ok(i) = self.search(key) {
            Some(&self.inner[i].1) 
        } else {
            None
        }
    }
}

#[macro_export]
macro_rules! stack_table {
    ($($key:expr => $val:expr,)*) => {
        unsafe {
            let mut ret = StackTable::new([
                $(($key, $val)),*
            ]);

            ret.get_inner_mut().sort_unstable_by_key(|&(k, _)| k);

            ret
        }
    };
}

#[test]
fn test_stack_table() {
    let map = stack_table!{
        "pis" => 0,
        "hest" => 2,
        "lort" => 1,
    };
    assert!(map.inner.is_sorted_by_key(|&(k, _)| k));
}