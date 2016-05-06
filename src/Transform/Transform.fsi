/// Copyright (C) 2016 The Authors.
module Transform

open Point
open Vector

type Axis = X | Y | Z

[<Sealed>]
type Transformation =
    /// <summary>
    /// Multiply two transformations.
    /// </summary>
    /// <param name=s>The first transformation.</param>
    /// <param name=t>The second transformation.</param>
    /// <returns>The resulting transformation.</returns>
    static member (*) : s:Transformation * t:Transformation -> Transformation

    /// <summary>
    /// Multiply a point by a transformation.
    /// </summary>
    /// <param name=p>The point.</param>
    /// <param name=t>The transformation.</param>
    /// <returns>The resulting point.</returns>
    static member (*) : p:Point * t:Transformation -> Point

    /// <summary>
    /// Multiply a vector by a transformation.
    /// </summary>
    /// <param name=v>The vector.</param>
    /// <param name=t>The transformation.</param>
    /// <returns>The resulting vector.</returns>
    static member (*) : v:Vector * t:Transformation -> Vector

/// <summary>
/// Construct an empty transformation, i.e. the identity transformation.
/// </summary>
/// <returns>An empty transformation.</returns>
val empty : Transformation

/// <summary>
/// Construct a translation transformation.
/// </summary>
/// <param name=x>The x component of the transformation.</param>
/// <param name=y>The y component of the transformation.</param>
/// <param name=z>The z component of the transformation.</param>
/// <returns>The translation transformation.</returns>
val translate : x:float -> y:float -> z:float -> Transformation

/// <summary>
/// Construct a scaling transformation.
/// </summary>
/// <param name=x>The x component of the transformation.</param>
/// <param name=y>The y component of the transformation.</param>
/// <param name=z>The z component of the transformation.</param>
/// <returns>The scaling transformation.</returns>
val scale : x:float -> y:float -> z:float -> Transformation

/// <summary>
/// Construct a mirroring transformation.
/// </summary>
/// <param name=a>The axis to mirror.</param>
/// <returns>The mirroring transformation.</returns>
val mirror : a:Axis -> Transformation

/// <summary>
/// Construct a rotation transformation.
/// </summary>
/// <param name=a>The axis to rotate around.</param>
/// <param name=t>The angle to rotate in radians.</param>
/// <returns>The rotation transformation.</returns>
val rotate : a:Axis -> t:float -> Transformation

/// <summary>
/// Construct a shearing transformation.
/// </summary>
/// <param name=a>The first axis to shear.</param>
/// <param name=b>The second axis to shear.</param>
/// <param name=d>The distance to shear.</param>
/// <returns>The shearing transformation.</returns>
val shear : a:Axis -> b:Axis -> d:float -> Transformation

/// <summary>
/// Transpose a transformation.
/// </summary>
/// <param name=t>The transformation to transpose.</param>
/// <returns>The transposed transformation.</returns>
val transpose : t:Transformation -> Transformation

/// <summary>
/// Get the inverse of a transformation.
/// </summary>
/// <remarks>
/// This implementation is based on the Laplace Expansion Theorem.
/// <see>http://stackoverflow.com/questions/2624422</see>
/// </remarks>
/// <param name=t>The transformation to inverse.</param>
/// <returns>The inverse of the transformation.</returns>
val inverse : t:Transformation -> Transformation

/// <summary>
/// Merge a list of transformations.
/// </summary>
/// <param name=ts>The transformations to merge.</param>
/// <returns>The merged transformation.</returns>
val merge : ts:Transformation list -> Transformation
