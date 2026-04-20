#define_import_path bevy_pbr::utils

#import bevy_pbr::rgb9e5
#import bevy_render::maths::{PI, PI_2, orthonormalize}

/* multi-line comment
   multi-line comment
 */

#ifdef
const SPIRAL_OFFSET_0_ = vec2<f32>(-0.7071,  0.7071);
#else
const SPIRAL_OFFSET_0_ = vec2<f32>(-0.7071,  0.7071);
#endif

// Convert direction vector to cube face UV
struct CubeUV {
    uv: vec2f,
    face: u32,
}

fn dir_to_cube_uv(dir: vec3f) -> CubeUV {
    let abs_dir = abs(dir);
    var face: u32 = 0u;
    var uv: vec2f = vec2f(0.0);

    // Find the dominant axis to determine face
    if (abs_dir.x >= abs_dir.y && abs_dir.x >= abs_dir.z) {
        // X axis is dominant
        if (dir.x > 0.0) {
            face = 0u; // +X
            uv = vec2f(-dir.z, -dir.y) / dir.x;
        } else {
            face = 1u; // -X
            uv = vec2f(dir.z, -dir.y) / abs_dir.x;
        }
    } else if (abs_dir.y >= abs_dir.x && abs_dir.y >= abs_dir.z) {
        // Y axis is dominant
        if (dir.y > 0.0) {
            face = 2u; // +Y
            uv = vec2f(dir.x, dir.z) / dir.y;
        } else {
            face = 3u; // -Y
            uv = vec2f(dir.x, -dir.z) / abs_dir.y;
        }
    } else {
        // Z axis is dominant
        if (dir.z > 0.0) {
            face = 4u; // +Z
            uv = vec2f(dir.x, -dir.y) / dir.z;
        } else {
            face = 5u; // -Z
            uv = vec2f(-dir.x, -dir.y) / abs_dir.z;
        }
    }

    // Convert from [-1,1] to [0,1]
    return CubeUV(uv * 0.5 + 0.5, face);
}

// The Porter-Duff OVER operator on RGBA, correctly computing alpha of the
// result.
fn porter_duff_over(bg: vec4<f32>, fg: vec4<f32>) -> vec4<f32> {
    return vec4<f32>(mix(bg.rgb * bg.a, fg.rgb, fg.a), bg.a + fg.a * (1.0 - bg.a));
}
