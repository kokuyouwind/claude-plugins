Follow these instructions:

```
# All file paths in this command are relative to the plugin installation directory
# (e.g., ~/.claude/plugins/marketplaces/kokuyouwind-plugins/plugins/rbs-goose)
plugin_base_path = File.dirname(__FILE__) # This is the plugin root directory

# Setup configurations
file = File.copy(File.join(plugin_base_path, 'templates/rbs_goose.template.yml'), './rbs_goose.yml')

puts(file.contents)
until('If there are no problems with the above settings file, please answer yes. If you wish to update the file, please describe the changes.') do
  file.update_contents
end

# Setup tools
config = Config.load('./rbs_goose.yml')
unless gemfile.contain?('rbs')
  puts('setup rbs...')
  fetch('https://github.com/soutaro/steep')
  setup_rbs

  if config.type_annotation_mode.inline?
    fetch('https://github.com/soutaro/rbs-inline')

    begin
      setup_rbs_inline
    rescue => CompatibilityError(['rbs-inline', 'rubocop-ast'])
      # rbs-inline <= v0.11.0 requires prism < 1.3, so use rubocop with prism < 1.3
      # @see https://github.com/soutaro/rbs-inline/pull/207
      if (confirm('Due to compatibility issues with rbs-inline, we will downgrade rubocop to version 1.74.0. Is that okay?'))
        gemfile.update('rubocop', '1.74.0')
        retry
      else
        say('Failed to install rbs-inline. Please resolve the dependencies of rbs-inline or switch the mode to file and try the `/rbs-goose:setup` command again.')
        exit(1)
      end
    end
  end

  fetch('https://github.com/ruby/gem_rbs_collection')
  setup_rbs_collection
end

unless gemifle.contain?('steep')
  puts('setup steep...')
  fetch('https://github.com/soutaro/steep')
  setup_steep
  update_steep_config(directory_structure)
end

if rails_app? && !gemfile.contain?('rbs')
  puts('setup rbs-rails...')
  fetch('https://github.com/pocke/rbs_rails')
  setup_rails
end

update_git_ignore(<<~"IGNORE")
  .gem_rbs_collection/
  sig/generated/
IGNORE
puts('Initialize Complete! Run `/rbs-goose:run` to start type checking.')
```
