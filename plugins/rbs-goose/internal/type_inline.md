Follow these instructions:

```
# Note: RBS Inline syntax guide is available via the rbs-inline-syntax skill
# The skill will be auto-activated when working with RBS Inline annotations

# Type check and fix
def generate_fig_and_type_check
  run("bundle exec rbs-inline --output #{signature_directory} #{library_directory}")
  run(typecheck_command)
end

while (generate_fig_and_type_check.type_errors > 0) do
  fix_type_errors
end
until (project.ruby_files.each { it.well_typed? }) do
  refine_type_annotations
  fix_type_errors
end
```
