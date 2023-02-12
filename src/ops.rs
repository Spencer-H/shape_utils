use super::SDF;

use bevy::math::*;

#[derive(Copy, Clone)]
pub struct Elongate<V, S> {
    pub sdf: S,
    pub scales: V,
}

impl<V, S> Elongate<V, S> {
    pub fn new(sdf: S, scales: V) -> Self {
        Self { sdf, scales }
    }
}

impl<S> SDF<Vec3, ()> for &Elongate<Vec3, S>
where
    S: SDF<Vec3, (), Item = f32> + Copy,
{
    type Item = f32;
    fn dist(self, point: Vec3) -> Self::Item {
        let q = point.abs() - self.scales;
        q.x.max(q.y).max(q.z).min(0.0) + self.sdf.dist(q.max(Vec3::ZERO))
    }
}

impl<S> SDF<Vec3A, ()> for &Elongate<Vec3A, S>
where
    S: SDF<Vec3A, (), Item = f32> + Copy,
{
    type Item = f32;
    fn dist(self, point: Vec3A) -> Self::Item {
        let q = point.abs() - self.scales;
        q.x.max(q.y).max(q.z).min(0.0) + self.sdf.dist(q.max(Vec3A::ZERO))
    }
}

#[derive(Copy, Clone)]
pub struct UElongate<V, S, U> {
    pub sdf: S,
    pub scales: V,
    _pd: std::marker::PhantomData<U>,
}

impl<V, S, U> UElongate<V, S, U>
where
    S: SDF<V, U>,
{
    pub fn new(sdf: S, scales: V) -> Self {
        Self {
            sdf,
            scales,
            _pd: std::marker::PhantomData,
        }
    }
}

impl<'a, S, U: 'a> SDF<Vec3, U> for &UElongate<Vec3, S, U>
where
    S: SDF<Vec3, U, Item = (f32, &'a U)> + Copy,
{
    type Item = (f32, &'a U);
    fn dist(self, point: Vec3) -> Self::Item {
        let q = point.abs() - self.scales;
        let (d, u) = self.sdf.dist(q.max(Vec3::ZERO));
        (q.x.max(q.y).max(q.z).min(0.0) + d, u)
    }
}

impl<'a, S, U: 'a> SDF<Vec3A, U> for &UElongate<Vec3A, S, U>
where
    S: SDF<Vec3A, U, Item = (f32, &'a U)> + Copy,
{
    type Item = (f32, &'a U);
    fn dist(self, point: Vec3A) -> Self::Item {
        let q = point.abs() - self.scales;
        let (d, u) = self.sdf.dist(q.max(Vec3A::ZERO));
        (q.x.max(q.y).max(q.z).min(0.0) + d, u)
    }
}

#[derive(Copy, Clone)]
pub struct Round<S> {
    pub sdf: S,
    pub radius: f32,
}

impl<S> Round<S> {
    pub fn new(sdf: S, radius: f32) -> Self {
        Self { sdf, radius }
    }
}

impl<S> SDF<Vec3, ()> for &Round<S>
where
    S: SDF<Vec3, (), Item = f32> + Copy,
{
    type Item = f32;
    fn dist(self, point: Vec3) -> Self::Item {
        self.sdf.dist(point) - self.radius
    }
}

impl<S> SDF<Vec3A, ()> for &Round<S>
where
    S: SDF<Vec3A, (), Item = f32> + Copy,
{
    type Item = f32;
    fn dist(self, point: Vec3A) -> Self::Item {
        self.sdf.dist(point) - self.radius
    }
}

#[derive(Copy, Clone)]
pub struct URound<S, U> {
    pub sdf: S,
    pub radius: f32,
    _pd: std::marker::PhantomData<U>,
}

impl<S, U> URound<S, U> {
    pub fn new(sdf: S, radius: f32) -> Self {
        Self {
            sdf,
            radius,
            _pd: std::marker::PhantomData,
        }
    }
}

impl<'a, S, U: 'a> SDF<Vec3, U> for &URound<S, U>
where
    S: SDF<Vec3, U, Item = (f32, &'a U)> + Copy,
{
    type Item = (f32, &'a U);
    fn dist(self, point: Vec3) -> Self::Item {
        let (d, u) = self.sdf.dist(point);
        (d - self.radius, u)
    }
}

impl<'a, S, U: 'a> SDF<Vec3A, U> for &URound<S, U>
where
    S: SDF<Vec3A, U, Item = (f32, &'a U)> + Copy,
{
    type Item = (f32, &'a U);
    fn dist(self, point: Vec3A) -> Self::Item {
        let (d, u) = self.sdf.dist(point);
        (d - self.radius, u)
    }
}

#[derive(Copy, Clone)]
pub struct Onion<S> {
    pub sdf: S,
    pub thickness: f32,
}

impl<S> Onion<S> {
    pub fn new(sdf: S, thickness: f32) -> Self {
        Self { sdf, thickness }
    }
}

impl<S> SDF<Vec3, ()> for &Onion<S>
where
    S: SDF<Vec3, (), Item = f32> + Copy,
{
    type Item = f32;
    fn dist(self, point: Vec3) -> Self::Item {
        self.sdf.dist(point).abs() - self.thickness
    }
}

impl<S> SDF<Vec3A, ()> for &Onion<S>
where
    S: SDF<Vec3A, (), Item = f32> + Copy,
{
    type Item = f32;
    fn dist(self, point: Vec3A) -> Self::Item {
        self.sdf.dist(point).abs() - self.thickness
    }
}

#[derive(Copy, Clone)]
pub struct UOnion<S, U> {
    pub sdf: S,
    pub thickness: f32,
    pub data: U,
}

impl<S, U> UOnion<S, U> {
    fn new(sdf: S, thickness: f32, data: U) -> Self {
        Self {
            sdf,
            thickness,
            data,
        }
    }
}

impl<'a, S, U: 'a> SDF<Vec3, U> for &UOnion<S, U>
where
    S: SDF<Vec3, U, Item = (f32, &'a U)> + Copy,
{
    type Item = (f32, &'a U);
    fn dist(self, point: Vec3) -> Self::Item {
        let (d, u) = self.sdf.dist(point);
        (d.abs() - self.thickness, u)
    }
}

impl<'a, S, U: 'a> SDF<Vec3A, U> for &UOnion<S, U>
where
    S: SDF<Vec3A, U, Item = (f32, &'a U)> + Copy,
{
    type Item = (f32, &'a U);
    fn dist(self, point: Vec3A) -> Self::Item {
        let (d, u) = self.sdf.dist(point);
        (d.abs() - self.thickness, u)
    }
}

#[derive(Clone, Copy)]
pub struct Union<S> {
    pub sdf1: S,
    pub sdf2: S,
}

impl<S> Union<S> {
    pub fn new(sdf1: S, sdf2: S) -> Self {
        Self { sdf1, sdf2 }
    }
}

impl<S> SDF<Vec3, ()> for &Union<S>
where
    S: SDF<Vec3, (), Item = f32> + Copy,
{
    type Item = S::Item;
    fn dist(self, point: Vec3) -> Self::Item {
        self.sdf1.dist(point).min(self.sdf2.dist(point))
    }
}

impl<S> SDF<Vec3A, ()> for &Union<S>
where
    S: SDF<Vec3A, (), Item = f32> + Copy,
{
    type Item = S::Item;
    fn dist(self, point: Vec3A) -> Self::Item {
        self.sdf1.dist(point).min(self.sdf2.dist(point))
    }
}

#[derive(Clone, Copy)]
pub struct UUnion<S, U> {
    pub sdf1: S,
    pub sdf2: S,
    _pd: std::marker::PhantomData<U>,
}

impl<S, U> UUnion<S, U> {
    pub fn new(sdf1: S, sdf2: S) -> Self {
        Self {
            sdf1,
            sdf2,
            _pd: std::marker::PhantomData,
        }
    }
}

impl<'a, S, U: 'a> SDF<Vec3, U> for UUnion<S, U>
where
    S: SDF<Vec3, U, Item = (f32, &'a U)> + Copy,
{
    type Item = (f32, &'a U);
    fn dist(self, point: Vec3) -> Self::Item {
        let (d1, u1) = self.sdf1.dist(point);
        let (d2, u2) = self.sdf2.dist(point);
        if d1 >= d2 {
            (d1, u1)
        } else {
            (d2, u2)
        }
    }
}

impl<'a, S, U: 'a> SDF<Vec3A, U> for UUnion<S, U>
where
    S: SDF<Vec3A, U, Item = (f32, &'a U)> + Copy,
{
    type Item = (f32, &'a U);
    fn dist(self, point: Vec3A) -> Self::Item {
        let (d1, u1) = self.sdf1.dist(point);
        let (d2, u2) = self.sdf2.dist(point);
        if d1 >= d2 {
            (d1, u1)
        } else {
            (d2, u2)
        }
    }
}

// TODO: Subtraction, Intersection, documentation
