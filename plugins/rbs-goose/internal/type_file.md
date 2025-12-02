Follow these instructions:

```
# Note: RBS syntax guide is available via the rbs-syntax skill
# The skill will be auto-activated when working with RBS type definitions

while (typecheck_command.run.type_errors > 0) do
  fix_type_errors
end
until (project.signatures.each { it.well_typed? }) do
  refine_signatures
  fix_type_errors
end
```
