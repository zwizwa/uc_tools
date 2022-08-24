#ifndef ELFUTILS_TAGS_H
#define ELFUTILS_TAGS_H

// Iterator macros for some of the elfutils symbolic tags.

// This is the list that occurs in current CM3 image.
// I did the tag number -> name mapping using /usr/include/llvm-3.8/llvm/Support/Dwarf.def
// But it is also in /usr/include/dwarf.h from libdw-dev

#define FOR_DW_TAG(m)                           \
    m(compile_unit)                             \
    m(base_type)                                \
    m(typedef)                                  \
    m(structure_type)                           \
    m(member)                                   \
    m(subprogram)                               \
    m(formal_parameter)                         \
    m(volatile_type)                            \
    m(enumerator)                               \
    m(enumeration_type)                         \
    m(pointer_type)                             \
    m(const_type)                               \
    m(subroutine_type)                          \
    m(array_type)                               \
    m(subrange_type)                            \
    m(union_type)                               \
    m(variable)                                 \
    m(lexical_block)                            \
    m(label)                                    \
    m(inlined_subroutine)                       \
    m(unspecified_parameters)                   \
    m(GNU_call_site)                            \
    m(GNU_call_site_parameter)                  \
    m(atomic_type)                              \

#define FOR_DW_AT(m)                            \
    m(producer)                                 \
    m(language)                                 \
    m(name)                                     \
    m(comp_dir)                                 \
    m(ranges)                                   \
    m(low_pc)                                   \
    m(high_pc)                                  \
    m(stmt_list)                                \
    m(byte_size)                                \
    m(bit_offset)                               \
    m(bit_size)                                 \
    m(encoding)                                 \
    m(location)                                 \
    m(decl_file)                                \
    m(decl_line)                                \
    m(decl_column)                              \
    m(type)                                     \
    m(data_member_location)                     \
    m(sibling)                                  \
    m(const_value)                              \
    m(inline)                                   \
    m(prototyped)                               \
    m(upper_bound)                              \
    m(abstract_origin)                          \
    m(artificial)                               \
    m(declaration)                              \
    m(external)                                 \
    m(frame_base)                               \
    m(entry_pc)                                 \
    m(call_file)                                \
    m(call_line)                                \
    m(specification)                            \
    m(GNU_call_site_value)                      \
    m(GNU_call_site_target)                     \
    m(GNU_tail_call)                            \
    m(GNU_all_tail_call_sites)                  \
    m(GNU_all_call_sites)                       \
    m(GNU_macros)                               \


#endif
