| program | opcode | operand | interpretatie |
|---------|--------|---------|---------------| 
| 2,4     | bst    | a       | b = a % 8     |
| 1,1     | bxl    | 1       | b = b ^ 1     |
| 7,5     | cdv    | b       | c = a >> b    |
| 1,5     | bxl    | 5       | b = b ^ 5     |
| 4,1     | bxc    | ?       | b = b ^ c     |
| 5, 5    | out    | b       | print b % 8   |
| 0, 3    | adv    | 3       | a = a >> 3    |
| 3, 0    | jnz    | 0       | ip = 0        |

```