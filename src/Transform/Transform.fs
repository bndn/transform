/// Copyright (C) 2016 The Authors.
module Transform

type Axis = X | Y | Z

type Transformation =
    | T of x:(float * float * float * float) *
           y:(float * float * float * float) *
           z:(float * float * float * float) *
           d:(float * float * float * float)

    /// <summary>
    /// Multiply two transformations.
    /// </summary>
    /// <param name=s>The first transformation.</param>
    /// <param name=t>The second transformation.</param>
    /// <returns>The resulting transformation.</returns>
    static member (*) (s, t) =
        let (T((s11, s12, s13, s14),
               (s21, s22, s23, s24),
               (s31, s32, s33, s34),
               (s41, s42, s43, s44))) = s

        let (T((t11, t12, t13, t14),
               (t21, t22, t23, t24),
               (t31, t32, t33, t34),
               (t41, t42, t43, t44))) = t

        let u11 = s11 * t11 + s12 * t21 + s13 * t31 + s14 * t41
        let u21 = s21 * t11 + s22 * t21 + s23 * t31 + s24 * t41
        let u31 = s31 * t11 + s32 * t21 + s33 * t31 + s34 * t41
        let u41 = s41 * t11 + s42 * t21 + s43 * t31 + s44 * t41

        let u12 = s11 * t12 + s12 * t22 + s13 * t32 + s14 * t42
        let u22 = s21 * t12 + s22 * t22 + s23 * t32 + s24 * t42
        let u32 = s31 * t12 + s32 * t22 + s33 * t32 + s34 * t42
        let u42 = s41 * t12 + s42 * t22 + s43 * t32 + s44 * t42

        let u13 = s11 * t13 + s12 * t23 + s13 * t33 + s14 * t43
        let u23 = s21 * t13 + s22 * t23 + s23 * t33 + s24 * t43
        let u33 = s31 * t13 + s32 * t23 + s33 * t33 + s34 * t43
        let u43 = s41 * t13 + s42 * t23 + s43 * t33 + s44 * t43

        let u14 = s11 * t14 + s12 * t24 + s13 * t34 + s14 * t44
        let u24 = s21 * t14 + s22 * t24 + s23 * t34 + s24 * t44
        let u34 = s31 * t14 + s32 * t24 + s33 * t34 + s34 * t44
        let u44 = s41 * t14 + s42 * t24 + s43 * t34 + s44 * t44

        T((u11, u12, u13, u14),
          (u21, u22, u23, u24),
          (u31, u32, u33, u34),
          (u41, u42, u43, u44))

    /// <summary>
    /// Multiply a point by a transformation.
    /// </summary>
    /// <param name=p>The point.</param>
    /// <param name=t>The transformation.</param>
    /// <returns>The resulting point.</returns>
    static member (*) (p, t) =
        let (T((a, b, c, d),
               (e, f, g, h),
               (i, j, k, l),
               (_, _, _, _))) = t

        let (x, y, z) = Point.getCoord p

        Point.make
            (x * a + y * b + z * c + d)
            (x * e + y * f + z * g + h)
            (x * i + y * j + z * k + l)

    /// <summary>
    /// Multiply a vector by a transformation.
    /// </summary>
    /// <param name=v>The vector.</param>
    /// <param name=t>The transformation.</param>
    /// <returns>The resulting vector.</returns>
    static member (*) (v, t) =
        let (T((a, b, c, _),
               (e, f, g, _),
               (i, j, k, _),
               (_, _, _, _))) = t

        let (x, y, z) = Vector.getCoord v

        Vector.make
            (x * a + y * b + z * c)
            (x * e + y * f + z * g)
            (x * i + y * j + z * k)

/// <summary>
/// Construct an empty transformation, i.e. the identity transformation.
/// </summary>
/// <returns>An empty transformation.</returns>
let empty =
    T((1., 0., 0., 0.),
      (0., 1., 0., 0.),
      (0., 0., 1., 0.),
      (0., 0., 0., 1.))

/// <summary>
/// Construct a translation transformation.
/// </summary>
/// <param name=x>The x component of the transformation.</param>
/// <param name=y>The y component of the transformation.</param>
/// <param name=z>The z component of the transformation.</param>
/// <returns>The translation transformation.</returns>
let translate x y z =
    T((1., 0., 0., x),
      (0., 1., 0., y),
      (0., 0., 1., z),
      (0., 0., 0., 1.))

/// <summary>
/// Construct a scaling transformation.
/// </summary>
/// <param name=x>The x component of the transformation.</param>
/// <param name=y>The y component of the transformation.</param>
/// <param name=z>The z component of the transformation.</param>
/// <returns>The scaling transformation.</returns>
let scale x y z =
    T((x,  0., 0., 0.),
      (0., y,  0., 0.),
      (0., 0., z,  0.),
      (0., 0., 0., 1.))

/// <summary>
/// Construct a mirroring transformation.
/// </summary>
/// <param name=a>The axis to mirror.</param>
/// <returns>The mirroring transformation.</returns>
let mirror a =
    let (x, y, z) =
        match a with
        | X -> (-1., 1., 1.)
        | Y -> (1., -1., 1.)
        | Z -> (1., 1., -1.)

    T((x,  0., 0., 0.),
      (0., y,  0., 0.),
      (0., 0., z,  0.),
      (0., 0., 0., 1.))

/// <summary>
/// Construct a rotation transformation.
/// </summary>
/// <param name=a>The axis to rotate around.</param>
/// <param name=t>The angle to rotate in radians.</param>
/// <returns>The rotation transformation.</returns>
let rotate a t =
    let ct = cos t
    let st = sin t

    match a with
    | X -> T((1., 0.,  0., 0.),
             (0., ct, -st, 0.),
             (0., st,  ct, 0.),
             (0., 0.,  0., 1.))

    | Y -> T((ct,  0., st, 0.),
             (0.,  1., 0., 0.),
             (-st, 0., ct, 0.),
             (0.,  0., 0., 1.))

    | Z -> T((ct, -st, 0., 0.),
             (st,  ct, 0., 0.),
             (0.,  0., 1., 0.),
             (0.,  0., 0., 1.))

/// <summary>
/// Construct a shearing transformation.
/// </summary>
/// <param name=a>The first axis to shear.</param>
/// <param name=b>The second axis to shear.</param>
/// <param name=d>The distance to shear.</param>
/// <returns>The shearing transformation.</returns>
let shear a b d =
    let (xy, xz, yx, yz, zx, zy) =
        match (a, b) with
        | (X, Y) -> (d, 0., 0., 0., 0., 0.)
        | (X, Z) -> (0., d, 0., 0., 0., 0.)
        | (Y, X) -> (0., 0., d, 0., 0., 0.)
        | (Y, Z) -> (0., 0., 0., d, 0., 0.)
        | (Z, X) -> (0., 0., 0., 0., d, 0.)
        | (Z, Y) -> (0., 0., 0., 0., 0., d)
        | _ -> failwith "Invalid axis combination"

    T((1., yx, zx, 0.),
      (xy, 1., zy, 0.),
      (xz, yz, 1., 0.),
      (0., 0., 0., 1.))

/// <summary>
/// Transpose a transformation.
/// </summary>
/// <param name=t>The transformation to transpose.</param>
/// <returns>The transposed transformation.</returns>
let transpose t =
    let (T((a, b, c, d),
           (e, f, g, h),
           (i, j, k, l),
           (m, n, o, p))) = t

    T((a, e, i, m),
      (b, f, j, n),
      (c, g, k, o),
      (d, h, l, p))

/// <summary>
/// Get the inverse of a transformation.
/// </summary>
/// <remarks>
/// This implementation is based on the Laplace Expansion Theorem.
/// <see>http://stackoverflow.com/questions/2624422</see>
/// </remarks>
/// <param name=t>The transformation to inverse.</param>
/// <returns>The inverse of the transformation.</returns>
let inverse t =
    let (T((t11, t12, t13, t14),
           (t21, t22, t23, t24),
           (t31, t32, t33, t34),
           (t41, t42, t43, t44))) = t

    let s1 = t11 * t22 - t21 * t12
    let s2 = t11 * t23 - t21 * t13
    let s3 = t11 * t24 - t21 * t14
    let s4 = t12 * t23 - t22 * t13
    let s5 = t12 * t24 - t22 * t14
    let s6 = t13 * t24 - t23 * t14

    let c6 = t33 * t44 - t43 * t34
    let c5 = t32 * t44 - t42 * t34
    let c4 = t32 * t43 - t42 * t33
    let c3 = t31 * t44 - t41 * t44
    let c2 = t31 * t43 - t41 * t43
    let c1 = t31 * t42 - t41 * t42

    let d = 1.0 / (s1 * c6 - s2 * c5 + s3 * c4 + s4 * c3 - s5 * c2 + s6 * c1)

    let u11 = ( t22 * c6 - t23 * c5 + t24 * c4) * d
    let u12 = (-t12 * c6 + t13 * c5 - t14 * c4) * d
    let u13 = ( t42 * s6 - t43 * s5 + t44 * s4) * d
    let u14 = (-t32 * s6 + t33 * s5 - t34 * s4) * d

    let u21 = (-t21 * c6 + t23 * c3 - t24 * c2) * d
    let u22 = ( t11 * c6 - t13 * c3 + t14 * c2) * d
    let u23 = (-t41 * s6 + t43 * s3 - t44 * s2) * d
    let u24 = ( t31 * s6 - t33 * s3 + t34 * s2) * d

    let u31 = ( t21 * c5 - t22 * c3 + t24 * c1) * d
    let u32 = (-t11 * c5 + t12 * c3 - t14 * c1) * d
    let u33 = ( t41 * s5 - t42 * s3 + t44 * s1) * d
    let u34 = (-t31 * s5 + t32 * s3 - t34 * s1) * d

    let u41 = (-t21 * c4 + t22 * c2 - t23 * c1) * d
    let u42 = ( t11 * c4 - t12 * c2 + t13 * c1) * d
    let u43 = (-t41 * s4 + t42 * s2 - t43 * s1) * d
    let u44 = ( t31 * s4 - t32 * s2 + t33 * s1) * d

    T((u11, u12, u13, u14),
      (u21, u22, u23, u24),
      (u31, u32, u33, u34),
      (u41, u42, u43, u44))

/// <summary>
/// Merge a list of transformations.
/// </summary>
/// <param name=ts>The transformations to merge.</param>
/// <returns>The merged transformation.</returns>
let merge ts = List.fold (fun tm t -> tm * t) empty ts
