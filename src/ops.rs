//! This module contains different modifiers or operations
//! that can be applied to SDFs in order to change them in
//! some way. This could be warping the shape of the SDF in some
//! way or it could be some relation between two SDFs such as a
//! [Union] or [Subtraction]
//!
//! Many operations have both a 'dataless' and 'user data' version
//! The versions that have User data are prefixed with a U, such as
//! [UUnion] vs. [Union]

use super::Lerp;
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

impl<S> SDF<()> for &Elongate<Vec3, S>
where
    S: SDF<(), Item = f32> + Copy,
{
    type Item = f32;
    fn dist(self, point: Vec3) -> Self::Item {
        let q = point.abs() - self.scales;
        q.x.max(q.y).max(q.z).min(0.0) + self.sdf.dist(q.max(Vec3::ZERO))
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
    S: SDF<U>,
{
    pub fn new(sdf: S, scales: V) -> Self {
        Self {
            sdf,
            scales,
            _pd: std::marker::PhantomData,
        }
    }
}

impl<'a, S, U: 'a> SDF<U> for &UElongate<Vec3, S, U>
where
    S: SDF<U, Item = (f32, &'a U)> + Copy,
{
    type Item = (f32, &'a U);
    fn dist(self, point: Vec3) -> Self::Item {
        let q = point.abs() - self.scales;
        let (d, u) = self.sdf.dist(q.max(Vec3::ZERO));
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

impl<S> SDF<()> for &Round<S>
where
    S: SDF<(), Item = f32> + Copy,
{
    type Item = f32;
    fn dist(self, point: Vec3) -> Self::Item {
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

impl<'a, S, U: 'a> SDF<U> for &URound<S, U>
where
    S: SDF<U, Item = (f32, &'a U)> + Copy,
{
    type Item = (f32, &'a U);
    fn dist(self, point: Vec3) -> Self::Item {
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

impl<S> SDF<()> for &Onion<S>
where
    S: SDF<(), Item = f32> + Copy,
{
    type Item = f32;
    fn dist(self, point: Vec3) -> Self::Item {
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

impl<'a, S, U: 'a> SDF<U> for &UOnion<S, U>
where
    S: SDF<U, Item = (f32, &'a U)> + Copy,
{
    type Item = (f32, &'a U);
    fn dist(self, point: Vec3) -> Self::Item {
        let (d, u) = self.sdf.dist(point);
        (d.abs() - self.thickness, u)
    }
}

#[derive(Clone, Copy)]
pub struct Union<S1, S2> {
    pub sdf1: S1,
    pub sdf2: S2,
}

impl<S1, S2> Union<S1, S2> {
    pub fn new(sdf1: S1, sdf2: S2) -> Self {
        Self { sdf1, sdf2 }
    }
}

impl<S1, S2> SDF<()> for &Union<S1, S2>
where
    S1: SDF<(), Item = f32> + Copy,
    S2: SDF<(), Item = f32> + Copy,
{
    type Item = f32;
    fn dist(self, point: Vec3) -> Self::Item {
        self.sdf1.dist(point).min(self.sdf2.dist(point))
    }
}

#[derive(Clone, Copy)]
pub struct UUnion<S1, S2, U> {
    pub sdf1: S1,
    pub sdf2: S2,
    _pd: std::marker::PhantomData<U>,
}

impl<S1, S2, U> UUnion<S1, S2, U> {
    pub fn new(sdf1: S1, sdf2: S2) -> Self {
        Self {
            sdf1,
            sdf2,
            _pd: std::marker::PhantomData,
        }
    }
}

impl<'a, S1, S2, U: 'a> SDF<U> for &UUnion<S1, S2, U>
where
    S1: SDF<U, Item = (f32, &'a U)> + Copy,
    S2: SDF<U, Item = (f32, &'a U)> + Copy,
{
    type Item = (f32, &'a U);
    fn dist(self, point: Vec3) -> Self::Item {
        let (d1, u1) = self.sdf1.dist(point);
        let (d2, u2) = self.sdf2.dist(point);
        if d1 <= d2 {
            (d1, u1)
        } else {
            (d2, u2)
        }
    }
}

#[derive(Clone, Copy)]
pub struct Subtraction<S1, S2> {
    pub sdf1: S1,
    pub sdf2: S2,
}

impl<S1, S2> Subtraction<S1, S2> {
    pub fn new(sdf1: S1, sdf2: S2) -> Self {
        Self { sdf1, sdf2 }
    }
}

impl<S1, S2> SDF<()> for &Subtraction<S1, S2>
where
    S1: SDF<(), Item = f32> + Copy,
    S2: SDF<(), Item = f32> + Copy,
{
    type Item = f32;
    fn dist(self, point: Vec3) -> Self::Item {
        (-self.sdf2.dist(point)).max(self.sdf1.dist(point))
    }
}

#[derive(Clone, Copy)]
pub struct USubtraction<S1, S2, U> {
    pub sdf1: S1,
    pub sdf2: S2,
    _pd: std::marker::PhantomData<U>,
}

impl<S1, S2, U> USubtraction<S1, S2, U> {
    pub fn new(sdf1: S1, sdf2: S2) -> Self {
        Self {
            sdf1,
            sdf2,
            _pd: std::marker::PhantomData,
        }
    }
}

impl<'a, S1, S2, U: 'a> SDF<U> for &USubtraction<S1, S2, U>
where
    S1: SDF<U, Item = (f32, &'a U)> + Copy,
    S2: SDF<U, Item = (f32, &'a U)> + Copy,
{
    type Item = (f32, &'a U);
    fn dist(self, point: Vec3) -> Self::Item {
        let (d1, u1) = self.sdf1.dist(point);
        let (d2, u2) = self.sdf2.dist(point);
        if d1 >= -d2 {
            (d1, u1)
        } else {
            (-d2, u2)
        }
    }
}

#[derive(Copy, Clone)]
pub struct Intersection<S1, S2> {
    pub sdf1: S1,
    pub sdf2: S2,
}

impl<S1, S2> Intersection<S1, S2> {
    pub fn new(sdf1: S1, sdf2: S2) -> Self {
        Self { sdf1, sdf2 }
    }
}

impl<S1, S2> SDF<()> for &Intersection<S1, S2>
where
    S1: SDF<(), Item = f32> + Copy,
    S2: SDF<(), Item = f32> + Copy,
{
    type Item = f32;
    fn dist(self, point: Vec3) -> Self::Item {
        self.sdf1.dist(point).max(self.sdf2.dist(point))
    }
}

#[derive(Copy, Clone)]
pub struct UIntersection<S1, S2, U> {
    pub sdf1: S1,
    pub sdf2: S2,
    _pd: std::marker::PhantomData<U>,
}

impl<S1, S2, U> UIntersection<S1, S2, U> {
    pub fn new(sdf1: S1, sdf2: S2) -> Self {
        Self {
            sdf1,
            sdf2,
            _pd: std::marker::PhantomData,
        }
    }
}

impl<'a, S1, S2, U: 'a> SDF<U> for &UIntersection<S1, S2, U>
where
    S1: SDF<U, Item = (f32, &'a U)> + Copy,
    S2: SDF<U, Item = (f32, &'a U)> + Copy,
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

#[derive(Clone, Copy)]
pub struct SmoothUnion<S1, S2> {
    pub sdf1: S1,
    pub sdf2: S2,
    pub transition: f32,
}

impl<S1, S2> SmoothUnion<S1, S2> {
    pub fn new(sdf1: S1, sdf2: S2, transition: f32) -> Self {
        Self {
            sdf1,
            sdf2,
            transition,
        }
    }
}

impl<S1, S2> SDF<()> for &SmoothUnion<S1, S2>
where
    S1: SDF<(), Item = f32> + Copy,
    S2: SDF<(), Item = f32> + Copy,
{
    type Item = f32;
    fn dist(self, point: Vec3) -> Self::Item {
        let d1 = self.sdf1.dist(point);
        let d2 = self.sdf2.dist(point);
        let h = (0.5 + 0.5 * (d2 - d1) / self.transition).clamp(0.0, 1.0);
        d2.lerp(d1, h) - self.transition * h * (1.0 - h)
    }
}

#[derive(Clone, Copy)]
pub struct USmoothUnion<S1, S2, U> {
    pub sdf1: S1,
    pub sdf2: S2,
    pub transition: f32,
    _pd: std::marker::PhantomData<U>,
}

impl<S1, S2, U> USmoothUnion<S1, S2, U> {
    pub fn new(sdf1: S1, sdf2: S2, transition: f32) -> Self {
        Self {
            sdf1,
            sdf2,
            transition,
            _pd: std::marker::PhantomData,
        }
    }
}

impl<'a, S1, S2, U: 'a> SDF<U> for &USmoothUnion<S1, S2, U>
where
    S1: SDF<(), Item = (f32, &'a U)> + Copy,
    S2: SDF<(), Item = (f32, &'a U)> + Copy,
    U: Lerp + Copy,
{
    type Item = (f32, U);
    fn dist(self, point: Vec3) -> Self::Item {
        let (d1, u1) = self.sdf1.dist(point);
        let (d2, u2) = self.sdf2.dist(point);
        let h = (0.5 + 0.5 * (d2 - d1) / self.transition).clamp(0.0, 1.0);
        (
            d2.lerp(d1, h) - self.transition * h * (1.0 - h),
            u2.lerp(*u1, h),
        )
    }
}

#[derive(Clone, Copy)]
pub struct SmoothSubtraction<S1, S2> {
    pub sdf1: S1,
    pub sdf2: S2,
    pub transition: f32,
}

impl<S1, S2> SmoothSubtraction<S1, S2> {
    pub fn new(sdf1: S1, sdf2: S2, transition: f32) -> Self {
        Self {
            sdf1,
            sdf2,
            transition,
        }
    }
}

impl<S1, S2> SDF<()> for SmoothSubtraction<S1, S2>
where
    S1: SDF<(), Item = f32> + Copy,
    S2: SDF<(), Item = f32> + Copy,
{
    type Item = f32;
    fn dist(self, point: Vec3) -> Self::Item {
        let d1 = self.sdf1.dist(point);
        let d2 = self.sdf2.dist(point);
        let h = (0.5 - 0.5 * (d2 - d1) / self.transition).clamp(0.0, 1.0);
        d2.lerp(-d1, h) + self.transition * h * (1.0 - h)
    }
}

#[derive(Copy, Clone)]
pub struct USmoothSubtraction<S1, S2, U> {
    pub sdf1: S1,
    pub sdf2: S2,
    pub transition: f32,
    _pd: std::marker::PhantomData<U>,
}

impl<S1, S2, U> USmoothSubtraction<S1, S2, U> {
    pub fn new(sdf1: S1, sdf2: S2, transition: f32) -> Self {
        Self {
            sdf1,
            sdf2,
            transition,
            _pd: std::marker::PhantomData,
        }
    }
}

impl<'a, S1, S2, U: 'a> SDF<U> for USmoothSubtraction<S1, S2, U>
where
    S1: SDF<U, Item = (f32, &'a U)> + Copy,
    S2: SDF<U, Item = (f32, &'a U)> + Copy,
    U: Lerp + Copy,
{
    type Item = (f32, U);
    fn dist(self, point: Vec3) -> Self::Item {
        let (d1, u1) = self.sdf1.dist(point);
        let (d2, u2) = self.sdf2.dist(point);
        let h = (0.5 - 0.5 * (d2 + d1) / self.transition).clamp(0.0, 1.0);
        (
            d2.lerp(-d1, h) + self.transition * h * (1.0 - h),
            u2.lerp(*u1, h),
        )
    }
}

#[derive(Clone, Copy)]
pub struct SmoothIntersection<S1, S2> {
    pub sdf1: S1,
    pub sdf2: S2,
    pub transition: f32,
}

impl<S1, S2> SmoothIntersection<S1, S2> {
    pub fn new(sdf1: S1, sdf2: S2, transition: f32) -> Self {
        Self {
            sdf1,
            sdf2,
            transition,
        }
    }
}

impl<S1, S2> SDF<()> for SmoothIntersection<S1, S2>
where
    S1: SDF<(), Item = f32> + Copy,
    S2: SDF<(), Item = f32> + Copy,
{
    type Item = f32;
    fn dist(self, point: Vec3) -> Self::Item {
        let d1 = self.sdf1.dist(point);
        let d2 = self.sdf2.dist(point);
        let h = (0.5 - 0.5 * (d2 - d1) / self.transition).clamp(0.0, 1.0);
        d2.lerp(d1, h) + self.transition * h * (1.0 - h)
    }
}

#[derive(Clone, Copy)]
pub struct USmoothIntersection<S1, S2, U> {
    pub sdf1: S1,
    pub sdf2: S2,
    pub transition: f32,
    _pd: std::marker::PhantomData<U>,
}

impl<S1, S2, U> USmoothIntersection<S1, S2, U> {
    pub fn new(sdf1: S1, sdf2: S2, transition: f32) -> Self {
        Self {
            sdf1,
            sdf2,
            transition,
            _pd: std::marker::PhantomData,
        }
    }
}

impl<'a, S1, S2, U: 'a> SDF<U> for USmoothIntersection<S1, S2, U>
where
    S1: SDF<U, Item = (f32, &'a U)> + Copy,
    S2: SDF<U, Item = (f32, &'a U)> + Copy,
    U: Lerp + Copy,
{
    type Item = (f32, U);
    fn dist(self, point: Vec3) -> Self::Item {
        let (d1, u1) = self.sdf1.dist(point);
        let (d2, u2) = self.sdf2.dist(point);
        let h = (0.5 - 0.5 * (d2 - d1) / self.transition).clamp(0.0, 1.0);
        (
            d2.lerp(d1, h) + self.transition * h * (1.0 - h),
            u2.lerp(*u1, h),
        )
    }
}

#[derive(Copy, Clone)]
pub struct Translation<S> {
    pub sdf: S,
    pub translation: Vec3,
}

impl<S> Translation<S> {
    pub fn new(sdf: S, translation: Vec3) -> Self {
        Self { sdf, translation }
    }
}

impl<S, U> SDF<U> for Translation<S>
where
    S: SDF<U> + Copy,
{
    type Item = S::Item;
    fn dist(self, point: Vec3) -> Self::Item {
        self.sdf.dist(point - self.translation)
    }
}

#[derive(Copy, Clone)]
pub struct Rotation<S> {
    pub sdf: S,
    pub rotation: Quat,
}

impl<S> Rotation<S> {
    pub fn new(sdf: S, rotation: Quat) -> Self {
        Self { sdf, rotation }
    }
}

impl<S, U> SDF<U> for Rotation<S>
where
    S: SDF<U>,
{
    type Item = S::Item;
    fn dist(self, point: Vec3) -> Self::Item {
        self.sdf.dist(self.rotation.inverse().mul_vec3(point))
    }
}

#[derive(Copy, Clone)]
pub struct Scale<S> {
    pub sdf: S,
    pub scale: f32,
}

impl<S> Scale<S> {
    pub fn new(sdf: S, scale: f32) -> Self {
        Self { sdf, scale }
    }
}

impl<S> SDF<()> for Scale<S>
where
    S: SDF<(), Item = f32> + Copy,
{
    type Item = f32;
    fn dist(self, point: Vec3) -> Self::Item {
        self.sdf.dist(point / self.scale) * self.scale
    }
}

#[derive(Copy, Clone)]
pub struct UScale<S, U> {
    pub sdf: S,
    pub scale: f32,
    _pd: std::marker::PhantomData<U>,
}

impl<S, U> UScale<S, U> {
    pub fn new(sdf: S, scale: f32) -> Self {
        Self {
            sdf,
            scale,
            _pd: std::marker::PhantomData,
        }
    }
}

impl<'a, S, U: 'a> SDF<U> for UScale<S, U>
where
    S: SDF<U, Item = (f32, &'a U)> + Copy,
{
    type Item = (f32, &'a U);
    fn dist(self, point: Vec3) -> Self::Item {
        let (d, u) = self.sdf.dist(point / self.scale);
        (d * self.scale, u)
    }
}

/// Allows for construction of custom modifiers for SDF's using a passed closure.
/// The closure is expected to return the correct data type for the SDF.
/// This means it is up to the user to either return an `f32` or a `(f32, U)`
/// as appropriate.
///
/// # Example (No Userdata)
/// ```
/// # use bevy::math::*;
/// # use shape_utils::SDF;
/// # use shape_utils::obj::Sphere;
/// # use shape_utils::ops::SDFModifier;
/// let sphere = Sphere::new(1.5);
/// let displaced_sphere = SDFModifier::new(
///     &sphere,
///     |sdf, point| {
///         let d1 = sdf.dist(point);
///         let d2 = point.x.sin()*point.y.sin()*point.z.sin();
///         d1+d2
///     }
/// );
/// // This would have been outside the sphere before, but it no longer is.
/// assert!(displaced_sphere.dist(vec3(-1.0, -1.0, -1.0)) <= 0.0);
/// ```
#[derive(Copy, Clone)]
pub struct SDFModifier<S, F, I, U>
where
    S: SDF<U, Item = I>,
    F: Fn(S, Vec3) -> I,
{
    pub sdf: S,
    pub func: F,
    _pd: std::marker::PhantomData<U>,
}

impl<S, F, I, U> SDFModifier<S, F, I, U>
where
    S: SDF<U, Item = I>,
    F: Fn(S, Vec3) -> I,
{
    pub fn new(sdf: S, func: F) -> Self {
        Self {
            sdf,
            func,
            _pd: std::marker::PhantomData,
        }
    }
}

impl<S, F, U, I> SDF<U> for SDFModifier<S, F, I, U>
where
    S: SDF<U, Item = I>,
    F: Fn(S, Vec3) -> I,
{
    type Item = S::Item;
    fn dist(self, point: Vec3) -> Self::Item {
        (self.func)(self.sdf, point)
    }
}
