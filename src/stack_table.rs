//! A statically sized binary search table implementation

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
        {
            let inner = [
                $(($key, $val)),*
            ];
            unsafe {
                let mut ret = StackTable::new(inner);
                ret.get_inner_mut().sort_unstable_by_key(|&(k, _)| k);

                ret
            }
        }
    };
}

#[cfg(test)]
fn is_sorted<I: Ord>(mut i: impl Iterator<Item = I>) -> bool {
    let mut last = if let Some(f) = i.next() {
        f
    } else {
        return true
    };

    for next in i {
        if last > next {
            return false;
        }
        last = next;
    }
    true
}

#[test]
fn test_stack_table() {
    let map = stack_table!{
        "pis" => 0,
        "hest" => 2,
        "lort" => 1,
    };
    assert!(is_sorted(map.inner.iter().map(|&(k, _)| k)));
}
