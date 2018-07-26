Reindeer = Struct.new(:name, :top_speed, :active_duration, :rest_duration) do
  def initialize(*args)
    super(*args)
    @acc = {}
  end

  def calc_travel(second)
    second % (active_duration + rest_duration) < active_duration ? top_speed : 0
  end

  def travel(seconds)
    if seconds == 1 || @acc.key?(seconds - 1)
      @acc[seconds] = @acc.fetch(seconds - 1, 0) + calc_travel(seconds - 1)
      return @acc[seconds]
    end

    (0..seconds - 1).map { |sec| calc_travel sec }.reduce(:+)
  end
end

reindeer = File
           .readlines('./input.txt')
           .reduce([]) do |list, line|
  name, top_speed, active_duration, rest_duration =
    line.match(/^(\w+).* (\d+).* (\d+).* (\d+)/).captures
  list << Reindeer.new(name,
                       top_speed.to_i,
                       active_duration.to_i,
                       rest_duration.to_i)
end

travel_seconds = 2503
puts reindeer
  .map { |rein| [rein.name, rein.travel(travel_seconds)] }
  .max_by { |_name, score| score }
  .join ': '

scores = {}
(1..travel_seconds).each do |second|
  results = reindeer.map { |rein| [rein.name, rein.travel(second)] }

  _, max_distance = results.max_by { |_name, dist| dist }

  results
    .select { |_name, dist| dist == max_distance }
    .each { |name, _dist| scores[name] = scores.fetch(name, 0) + 1 }
end

puts scores.max_by { |_name, score| score }.join ': '
