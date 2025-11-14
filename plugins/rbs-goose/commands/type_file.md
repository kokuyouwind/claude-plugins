Follow these instructions:

```
# Load Documentation
fetch('https://github.com/ruby/rbs/blob/master/docs/syntax.md')

while (typecheck_command.run.type_errors > 0) do
  fix_type_errors
end
until (project.signatures.each { it.well_typed? }) do
  refine_signatures
  fix_type_errors
end
```
