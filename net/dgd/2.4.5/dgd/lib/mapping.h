static mapping mkmapping(mixed *indices, mixed *values);
static mixed *m_indices(mapping map);
static mixed *m_values(mapping map);
static mapping m_delete(mapping map, mixed elt);
static int m_sizeof(mapping map);
static varargs mapping
filter_mapping(mapping map, mixed func, mixed obj, mixed arg);
static varargs mapping
map_mapping(mapping map, mixed func, mixed obj, mixed arg);

# define INIT_MAPPING()
