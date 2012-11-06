namespace FSCL

module fscl =
    let get_global_id(dim:int) = 
        0
    let get_local_id(dim:int) =
        0
    let get_global_size(dim:int) =
        0
    let get_local_size(dim:int) =
        0
    let get_num_groups(dim:int) =
        0
    let get_group_id(dim:int) =
        0
    let get_global_offset(dim:int) =
        0
    let get_work_dim() =
        0

    type uchar = Char
    type uint = Int
    type size_t = Int
    type half = Double
    type ulong = Long
    
    (**
    type CLVector2<'T>(xv,yv) =
        let mutable xp:'T = xv
        let mutable yp:'T = yv

        new(xv) =
            CLVector2<'T>(xv,xv)

        member this.x 
            with get() = xp 
            and set v = xp <- v
        member this.y 
            with get() = yp 
            and set v = yp <- v
        member this.xx 
            with get() = new CLVector2<'T>(xp, xp)
        member this.yy 
            with get() = new CLVector2<'T>(yp, yp)
        member this.xy 
            with get() = new CLVector2<'T>(xp, yp) 
            and set (v:CLVector2<'T>) = 
                xp <- v.x
                yp <- v.y
        member this.yx 
            with get() = new CLVector2<'T>(yp, xp) 
            and set (v:CLVector2<'T>) = 
                xp <- v.y
                yp <- v.x
                
        member this.lo 
            with get() = this.x
            and set (v) = 
                this.x <- v
        member this.hi 
            with get() = this.y
            and set (v) = 
                this.y <- v
                
        member this.even 
            with get() = this.x
            and set (v) = 
                this.x <- v
        member this.odd 
            with get() = this.y
            and set (v) = 
                this.y <- v
                
    type CLVector3<'T>(xv,yv,zv) =
        inherit CLVector2<'T>(xv, yv)

        let mutable zp:'T = zv

        new(xv) =
            CLVector3<'T>(xv,xv,xv)

        member this.z
            with get() = zp 
            and set v = zp <- v
        member this.zz 
            with get() = new CLVector2<'T>(zp, zp)
        member this.xz 
            with get() = new CLVector2<'T>(this.x, this.z)
            and set (v:CLVector2<'T>) = 
                this.x <- v.x
                this.z <- v.y
        member this.yz 
            with get() = new CLVector2<'T>(this.y, this.z)
            and set (v:CLVector2<'T>) = 
                this.y <- v.y
                this.z <- v.y
        member this.zx 
            with get() = new CLVector2<'T>(this.z, this.x)
            and set (v:CLVector2<'T>) = 
                this.z <- v.x
                this.x <- v.y
        member this.zy 
            with get() = new CLVector2<'T>(this.z, this.y)
            and set (v:CLVector2<'T>) = 
                this.z <- v.x
                this.y <- v.y

                
        member this.lo 
            with get() = this.x
            and set (v) = 
                this.x <- v
        member this.hi 
            with get() = this.y
            and set (v) = 
                this.y <- v
                
        member this.even 
            with get() = this.x
            and set (v) = 
                this.x <- v
        member this.odd 
            with get() = this.y
            and set (v) = 
                this.y <- v

    type float2(v1:float, v2:float) =
        inherit CLVector2<float>(v1, v2)   

        static member (+) (v1:float2, v2:float2) =
            new float2(v1.x + v2.x, v1.y + v2.y)
        static member (+) (v1:CLVector2<'T>, v2:'T) =
            new CLVector2<'T>(v1.x + v2, v1.y + v2)
            
        static member (*) (v1:CLVector2<'T>, v2:CLVector2<'T>) =
            new CLVector2<'T>(v1.x + v2.x, v1.y + v2.y)
        static member (*) (v1:CLVector2<'T>, v2:'T) =
            new CLVector2<'T>(v1.x + v2, v1.y + v2)
            
        static member (/) (v1:CLVector2<'T>, v2:CLVector2<'T>) =
            new CLVector2<'T>(v1.x + v2.x, v1.y + v2.y)
        static member (/) (v1:CLVector2<'T>, v2:'T) =
            new CLVector2<'T>(v1.x + v2, v1.y + v2) 
            *)