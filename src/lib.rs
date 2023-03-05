//! Utilities for using [SDF] structures to create and modify shapes.
//!
//! This crate specifically uses [Bevy]'s Vec types for ease of
//! integration with the engine.
//!
//! This crate does not make any promises about the behavior of the
//! SDFs, for example, quite a few of the functions within this
//! crate will produce bound SDFs rather than true ones.
//!
//! The main advantage of this crate is the ability to associate
//! user specified data with the created objects which will be
//! preserved through various manipulations.
//!
//! # Example
//! ```
//! # use bevy::math::Vec3;
//! # use shape_utils::SDF;
//! # use shape_utils::obj::Sphere;
//! let sphere = Sphere::new(1.0); // Create a Sphere of radius 1
//! let inside = sphere.dist(Vec3::new(0.5, 0.5, 0.5));
//! let outside = sphere.dist(Vec3::new(1.5, 1.5, 1.5));
//! assert!(inside <= 0.0); // Inside is inside the sphere.
//! assert!(outside >= 0.0); // Outside is outside the sphere.
//! ```
//!
//! [Bevy]: https://bevyengine.org
//! [SDF]: https://wikipedia.org/wiki/Signed_distance_function

#![warn(missing_docs)]

use bevy::prelude::{Quat, Transform, Vec3};
use std::ops::{Add, Mul};

pub mod obj;
pub mod ops;

use ops::*;

/// The main trait that enables functionality within `shape_utils`
/// Every Object and Operation implements this trait, and accepts
/// anything that implements this trait in order to allow for
/// composition of various transforms.
pub trait SDF<U> {
    /// The return type of the SDF, usually either [f32] or (f32, U)
    type Item;
    /// Returns the distance between the provided point and the surface
    /// of this SDF. Positive for outside, negative for inside.
    /// This will also return any user data attached to the SDF.
    fn dist(self, point: Vec3) -> Self::Item;

    /// Creates a [Union] with the current SDF and a second SDF.
    /// Union is the non-userdata version. For working with userdata
    /// see [uunion] / [UUnion]
    ///
    /// # Example
    /// ```
    /// # use bevy::math::Vec3;
    /// # use shape_utils::SDF;
    /// # use shape_utils::obj::Sphere;
    /// # use shape_utils::obj::Torus;
    /// let sphere = Sphere::new(1.0);
    /// let torus = Torus::new(0.5, 1.0);
    /// let union = sphere.union(&torus);
    /// ```
    fn union<O: SDF<()>>(self, other: O) -> Union<Self, O>
    where
        Self: Sized + SDF<()> + Copy,
    {
        Union::new(self, other)
    }

    /// Creates a [UUnion] with the current SDF and a second SDF.
    /// This will be a combination of the two SDFs.
    /// UUnion is the userdata version. For working with non-userdata
    /// see [union] / [Union]
    ///
    /// # Example
    /// ```
    /// # use bevy::math::Vec3;
    /// # use shape_utils::SDF;
    /// # use shape_utils::obj::USphere;
    /// # use shape_utils::obj::UTorus;
    /// let sphere = USphere::new(1.0, 1);
    /// let torus = UTorus::new(0.5, 1.0, 2);
    /// let union = sphere.uunion(&torus);
    /// ```
    fn uunion<O: SDF<U>>(self, other: O) -> UUnion<Self, O, U>
    where
        Self: Sized + SDF<U> + Copy,
    {
        UUnion::new(self, other)
    }

    /// Creates a [Subtraction] with the current SDF and a second SDF.
    /// This will be the first SDF with any overlap from the second SDF removed.
    /// For working with userdata see [usubtract] / [USubtraction]
    ///
    /// # Example
    /// ```
    /// # use bevy::math::Vec3;
    /// # use shape_utils::SDF;
    /// # use shape_utils::obj::{Sphere, Box};
    /// let box = Box::new(
    fn subtract<O: SDF<()>>(self, other: O) -> Subtraction<Self, O>
    where
        Self: Sized + SDF<()> + Copy,
    {
        Subtraction::new(self, other)
    }

    /// Creates a [USubtraction] with the current SDF and a second SDF.
    /// This will be the first SDF with any overlap from the second SDF removed.
    fn usubtract<O: SDF<U>>(self, other: O) -> USubtraction<Self, O, U>
    where
        Self: Sized + SDF<U> + Copy,
    {
        USubtraction::new(self, other)
    }

    fn intersect<O: SDF<()>>(self, other: O) -> Intersection<Self, O>
    where
        Self: Sized + SDF<()> + Copy,
    {
        Intersection::new(self, other)
    }

    fn uintersect<O: SDF<U>>(self, other: O) -> UIntersection<Self, O, U>
    where
        Self: Sized + SDF<U> + Copy,
    {
        UIntersection::new(self, other)
    }

    fn smooth_union<O: SDF<()>>(self, other: O, transition: f32) -> SmoothUnion<Self, O>
    where
        Self: Sized + SDF<()> + Copy,
    {
        SmoothUnion::new(self, other, transition)
    }

    fn usmooth_union<O: SDF<U>>(self, other: O, transition: f32) -> USmoothUnion<Self, O, U>
    where
        Self: Sized + SDF<U> + Copy,
    {
        USmoothUnion::new(self, other, transition)
    }

    fn smooth_subtract<O: SDF<()>>(self, other: O, transition: f32) -> SmoothSubtraction<Self, O>
    where
        Self: Sized + SDF<()> + Copy,
    {
        SmoothSubtraction::new(self, other, transition)
    }

    fn usmooth_subtract<O: SDF<U>>(
        self,
        other: O,
        transition: f32,
    ) -> USmoothSubtraction<Self, O, U>
    where
        Self: Sized + SDF<U> + Copy,
    {
        USmoothSubtraction::new(self, other, transition)
    }

    fn smooth_intersect<O: SDF<()>>(self, other: O, transition: f32) -> SmoothIntersection<Self, O>
    where
        Self: Sized + SDF<()> + Copy,
    {
        SmoothIntersection::new(self, other, transition)
    }

    fn usmooth_intersect<O: SDF<U>>(
        self,
        other: O,
        transition: f32,
    ) -> USmoothIntersection<Self, O, U>
    where
        Self: Sized + SDF<U> + Copy,
    {
        USmoothIntersection::new(self, other, transition)
    }

    fn translate(self, translation: Vec3) -> Translation<Self>
    where
        Self: Sized + SDF<U> + Copy,
    {
        Translation::new(self, translation)
    }

    fn rotate(self, rotation: Quat) -> Rotation<Self>
    where
        Self: Sized + SDF<U> + Copy,
    {
        Rotation::new(self, rotation)
    }

    fn scale(self, scale: f32) -> Scale<Self>
    where
        Self: Sized + SDF<U> + Copy,
    {
        Scale::new(self, scale)
    }

    fn transform(self, transform: Transform) -> Scale<Rotation<Translation<Self>>>
    where
        Self: Sized + SDF<U> + Copy,
    {
        self.translate(transform.translation)
            .rotate(transform.rotation)
            .scale(transform.scale.max_element())
    }

    /// This function is a wrapper around [SDFModifier] to allow for easy chaining of modifiers.
    ///
    /// # Example
    /// ```
    /// # use bevy::math::*;
    /// # use shape_utils::SDF;
    /// # use shape_utils::obj::Sphere;
    /// # use shape_utils::ops::SDFModifier;
    /// let sphere = Sphere::new(1.5);
    /// assert!(sphere.dist(vec3(-1.0, -1.0, -1.0)) >= 0.0);
    /// let displaced_sphere = sphere.modify(
    ///     |sdf, point| {
    ///         let d1 = sdf.dist(point);
    ///         let d2 = point.x.sin()*point.y.sin()*point.z.sin();
    ///         d1+d2
    ///     }
    /// );
    /// // This would have been outside the sphere before, but it no longer is.
    /// assert!(displaced_sphere.dist(vec3(-1.0, -1.0, -1.0)) <= 0.0);
    /// ```
    fn modify<I, F>(self, func: F) -> SDFModifier<Self, F, I, U>
    where
        Self: Sized + SDF<U, Item = I> + Copy,
        F: Fn(Self, Vec3) -> I,
    {
        SDFModifier::new(self, func)
    }
}

pub trait Lerp {
    fn lerp(self, other: Self, t: f32) -> Self;
}

impl<T> Lerp for T
where
    T: Add<Output = T> + Mul<f32, Output = T>,
{
    fn lerp(self, other: Self, t: f32) -> Self {
        self * (1.0 - t) + other * t
    }
}
