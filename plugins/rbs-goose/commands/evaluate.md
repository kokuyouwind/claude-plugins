Follow these instructions:

```
# All file paths are relative to the plugin installation directory
# (e.g., ~/.claude/plugins/marketplaces/kokuyouwind-plugins/plugins/rbs-goose)
plugin_base_path = File.dirname(__FILE__) # This is the plugin root directory

# Evaluation Configuration
evaluation_repo = 'https://github.com/kokuyouwind/claude_rbs_goose_examples'
evaluation_dir = './tmp/rbs_goose_evaluation'

# Setup evaluation environment
follow_instruction(File.join(plugin_base_path, 'internal/evaluation/setup.md'))

# Run evaluation on all test cases
test_cases = Dir.glob(File.join(evaluation_dir, '*')).select { |path| File.directory?(path) }
results = []

test_cases.each do |test_case_path|
  test_case_name = File.basename(test_case_path)
  puts("Evaluating test case: #{test_case_name}")

  result = follow_instruction(File.join(plugin_base_path, 'internal/evaluation/run_test.md'), {
    test_case_path: test_case_path,
    test_case_name: test_case_name
  })

  results << result
end

# Score and report results
follow_instruction(File.join(plugin_base_path, 'internal/evaluation/score.md'), {
  results: results
})
```
