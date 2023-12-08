
pub trait IteratorExt<T> {
}

impl<I, T> IteratorExt<T> for I where I: Iterator<Item = T> {
}
