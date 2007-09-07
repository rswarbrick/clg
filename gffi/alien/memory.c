#include <stdint.h>

uint16_t gffi_get_uint_16_swapped (uint8_t* location, int offset)
{
  uint8_t buf[2];
  buf[1] = location[offset];
  buf[0] = location[offset + 1];
  return *(uint16_t*)buf;
}

void gffi_set_uint_16_swapped (uint8_t* location, int offset, uint16_t value)
{
  uint8_t *buf = (uint8_t*)&value;
  
  location[offset + 0] = buf[1];
  location[offset + 1] = buf[0];
}

uint32_t gffi_get_uint_32_swapped (uint8_t* location, int offset)
{
  uint8_t buf[4];
  buf[3] = location[offset];
  buf[2] = location[offset + 1];
  buf[1] = location[offset + 2];
  buf[0] = location[offset + 3];
  return *(uint32_t*)buf;
}

void gffi_set_uint_32_swapped (uint8_t* location, int offset, uint32_t value)
{
  uint8_t *buf = (uint8_t*)&value;
  
  location[offset + 0] = buf[3];
  location[offset + 1] = buf[2];
  location[offset + 2] = buf[1];
  location[offset + 3] = buf[0];
}

uint64_t gffi_get_uint_64_swapped (uint8_t* location, int offset)
{
  uint8_t buf[8];
  buf[7] = location[offset];
  buf[6] = location[offset + 1];
  buf[5] = location[offset + 2];
  buf[4] = location[offset + 3];
  buf[3] = location[offset + 4];
  buf[2] = location[offset + 5];
  buf[1] = location[offset + 6];
  buf[0] = location[offset + 7];
  return *(uint64_t*)buf;
}

void gffi_set_uint_64_swapped (uint8_t* location, int offset, uint64_t value)
{
  uint8_t *buf = (uint8_t*)&value;
  
  location[offset + 0] = buf[7];
  location[offset + 1] = buf[6];
  location[offset + 2] = buf[5];
  location[offset + 3] = buf[4];
  location[offset + 4] = buf[3];
  location[offset + 5] = buf[2];
  location[offset + 6] = buf[1];
  location[offset + 7] = buf[0];
}

int16_t gffi_get_int_16_swapped (int8_t* location, int offset)
{
  int8_t buf[2];
  buf[1] = location[offset];
  buf[0] = location[offset + 1];
  return *(int16_t*)buf;
}

void gffi_set_int_16_swapped (uint8_t* location, int offset, int16_t value)
{
  uint8_t *buf = (uint8_t*)&value;
  
  location[offset + 0] = buf[1];
  location[offset + 1] = buf[0];
}

int32_t gffi_get_int_32_swapped (int8_t* location, int offset)
{
  int8_t buf[4];
  buf[3] = location[offset];
  buf[2] = location[offset + 1];
  buf[1] = location[offset + 2];
  buf[0] = location[offset + 3];
  return *(int32_t*)buf;
}

void gffi_set_int_32_swapped (uint8_t* location, int offset, int32_t value)
{
  uint8_t *buf = (uint8_t*)&value;
  
  location[offset + 0] = buf[3];
  location[offset + 1] = buf[2];
  location[offset + 2] = buf[1];
  location[offset + 3] = buf[0];
}

int64_t gffi_get_int_64_swapped (int8_t* location, int offset)
{
  int8_t buf[8];
  buf[7] = location[offset];
  buf[6] = location[offset + 1];
  buf[5] = location[offset + 2];
  buf[4] = location[offset + 3];
  buf[3] = location[offset + 4];
  buf[2] = location[offset + 5];
  buf[1] = location[offset + 6];
  buf[0] = location[offset + 7];
  return *(int64_t*)buf;
}

void gffi_set_int_64_swapped (uint8_t* location, int offset, int64_t value)
{
  uint8_t *buf = (uint8_t*)&value;
  
  location[offset + 0] = buf[7];
  location[offset + 1] = buf[6];
  location[offset + 2] = buf[5];
  location[offset + 3] = buf[4];
  location[offset + 4] = buf[3];
  location[offset + 5] = buf[2];
  location[offset + 6] = buf[1];
  location[offset + 7] = buf[0];
}

float gffi_get_single_float_swapped (int8_t* location, int offset)
{
  int8_t buf[4];
  buf[3] = location[offset];
  buf[2] = location[offset + 1];
  buf[1] = location[offset + 2];
  buf[0] = location[offset + 3];
  return *(float*)buf;
}

void gffi_set_single_float_swapped (uint8_t* location, int offset, float value)
{
  uint8_t *buf = (uint8_t*)&value;
  
  location[offset + 0] = buf[3];
  location[offset + 1] = buf[2];
  location[offset + 2] = buf[1];
  location[offset + 3] = buf[0];
}

double gffi_get_double_float_swapped (int8_t* location, int offset)
{
  int8_t buf[8];
  buf[7] = location[offset];
  buf[6] = location[offset + 1];
  buf[5] = location[offset + 2];
  buf[4] = location[offset + 3];
  buf[3] = location[offset + 4];
  buf[2] = location[offset + 5];
  buf[1] = location[offset + 6];
  buf[0] = location[offset + 7];
  return *(double*)buf;
}

void gffi_set_double_float_swapped (uint8_t* location, int offset, double value)
{
  uint8_t *buf = (uint8_t*)&value;
  
  location[offset + 0] = buf[7];
  location[offset + 1] = buf[6];
  location[offset + 2] = buf[5];
  location[offset + 3] = buf[4];
  location[offset + 4] = buf[3];
  location[offset + 5] = buf[2];
  location[offset + 6] = buf[1];
  location[offset + 7] = buf[0];
}
