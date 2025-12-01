Follow these instructions:

```
# Run rbs-goose on a single test case and collect metrics
# Parameters: test_case_path, test_case_name

test_case_path = params[:test_case_path]
test_case_name = params[:test_case_name]

puts("\n" + "=" * 80)
puts("Running evaluation for: #{test_case_name}")
puts("=" * 80)

# Change to test case directory
original_dir = Dir.pwd
Dir.chdir(test_case_path)

# Initialize result object
result = {
  name: test_case_name,
  success: false,
  execution_time: 0,
  error_count_before: 0,
  error_count_after: 0,
  iterations: 0,
  generated_files: [],
  error_message: nil
}

begin
  start_time = Time.now

  # Check if this is a valid Ruby project
  unless File.exists?('Gemfile')
    result[:error_message] = "No Gemfile found - not a valid Ruby project"
    puts("ERROR: #{result[:error_message]}")
    return result
  end

  # Install dependencies
  puts("Installing dependencies...")
  run("bundle install")

  # Get initial error count (if config exists)
  if File.exists?('./rbs_goose.yml')
    config = Config.load('./rbs_goose.yml')
    typecheck_result = run(config.typecheck_command)
    result[:error_count_before] = typecheck_result.error_count
  end

  # Run rbs-goose
  puts("\nRunning /rbs-goose:run...")
  iteration_count = 0
  max_iterations = 10 # Safety limit

  # Track iterations by monitoring run command
  while iteration_count < max_iterations
    iteration_count += 1
    puts("Iteration #{iteration_count}...")

    # This would trigger the actual rbs-goose:run command
    # In practice, Claude will execute the run.md command
    run_result = run("/rbs-goose:run")

    # Check if type checking passes
    if File.exists?('./rbs_goose.yml')
      config = Config.load('./rbs_goose.yml')
      typecheck_result = run(config.typecheck_command)

      if typecheck_result.error_count == 0
        puts("Type checking passed!")
        break
      end

      result[:error_count_after] = typecheck_result.error_count
    end

    # If no progress after 3 iterations, break
    if iteration_count >= 3 && result[:error_count_after] == result[:error_count_before]
      puts("No progress made after #{iteration_count} iterations")
      break
    end
  end

  result[:iterations] = iteration_count
  result[:execution_time] = Time.now - start_time

  # Collect generated files
  if File.exists?('./sig')
    result[:generated_files] = Dir.glob('./sig/**/*.rbs')
  end

  # Check for inline annotations
  ruby_files = Dir.glob('./**/*.rb')
  ruby_files.each do |file|
    content = File.read(file)
    if content.include?('# @rbs')
      result[:generated_files] << file
    end
  end

  # Final type check
  if File.exists?('./rbs_goose.yml')
    config = Config.load('./rbs_goose.yml')
    typecheck_result = run(config.typecheck_command)
    result[:error_count_after] = typecheck_result.error_count
    result[:success] = typecheck_result.error_count == 0
  end

  puts("\nTest case completed:")
  puts("  Success: #{result[:success]}")
  puts("  Iterations: #{result[:iterations]}")
  puts("  Execution time: #{result[:execution_time].round(2)}s")
  puts("  Errors before: #{result[:error_count_before]}")
  puts("  Errors after: #{result[:error_count_after]}")
  puts("  Generated files: #{result[:generated_files].length}")

rescue => e
  result[:error_message] = e.message
  puts("ERROR: #{e.message}")
ensure
  # Return to original directory
  Dir.chdir(original_dir)
end

result
```
