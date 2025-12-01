Follow these instructions:

```
# Setup evaluation environment
# This script clones the evaluation repository and prepares it for testing

evaluation_repo = 'https://github.com/kokuyouwind/claude_rbs_goose_examples'
evaluation_dir = './tmp/rbs_goose_evaluation'

# Clean up previous evaluation directory if it exists
if File.exists?(evaluation_dir)
  puts("Cleaning up previous evaluation directory...")
  FileUtils.rm_rf(evaluation_dir)
end

# Create tmp directory if it doesn't exist
unless File.exists?('./tmp')
  FileUtils.mkdir_p('./tmp')
end

# Clone the evaluation repository
puts("Cloning evaluation repository from #{evaluation_repo}...")
run("git clone #{evaluation_repo} #{evaluation_dir}")

unless File.exists?(evaluation_dir)
  say("Failed to clone evaluation repository. Please check your internet connection and repository access.")
  exit(1)
end

puts("Evaluation environment setup complete.")
puts("Test cases found in: #{evaluation_dir}")

# List available test cases
test_cases = Dir.glob(File.join(evaluation_dir, '*')).select { |path| File.directory?(path) && !File.basename(path).start_with?('.') }
puts("\nAvailable test cases:")
test_cases.each do |test_case|
  puts("  - #{File.basename(test_case)}")
end
```
