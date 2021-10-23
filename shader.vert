#version 450

// Based on https://vulkan-tutorial.com/en/Drawing_a_triangle/Graphics_pipeline_basics/Shader_modules

vec2 positions[3] = vec2[](vec2(0.0, -0.5), vec2(0.5, 0.5), vec2(-0.5, 0.5));

void main() {
  gl_Position = vec4(positions[gl_VertexIndex], 0.0, 1.0);
}
