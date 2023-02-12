mod obj;
mod ops;

pub trait SDF<V, U> {
    type Item;
    fn dist(self, point: V) -> Self::Item;
}
