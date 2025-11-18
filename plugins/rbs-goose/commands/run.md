Follow these instructions:

```
# All file paths are relative to the plugin installation directory
# (e.g., ~/.claude/plugins/marketplaces/kokuyouwind-plugins/plugins/rbs-goose)
plugin_base_path = File.dirname(__FILE__) # This is the plugin root directory

# First Run Setup
unless File.exists?('./rbs_goose.yml')
  follow_instruction(File.join(plugin_base_path, 'commands/setup.md'))
  return
end

config = Config.load('./rbs_goose.yml')

case config.type_annotation_mode
when :inline
  follow_instruction(File.join(plugin_base_path, 'internal/type_inline.md'))
when :file
  follow_instruction(File.join(plugin_base_path, 'internal/type_file.md'))
end
```
