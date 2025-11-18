Follow these instructions:

```
# Load Documentation
fetch('https://github.com/soutaro/rbs-inline/wiki/Syntax-guide')

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
