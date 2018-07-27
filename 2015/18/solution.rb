world =
  File
  .readlines('./input.txt')
  .map(&:strip)
  .map(&:each_char)
  .each_with_object({}).with_index do |(line, hash), y|
    line.each.with_index { |cell, x| hash[[x, y]] = cell }
  end

def draw_world(world)
  max = world.keys.max_by { |c| c.reduce(:+) }
  (0..max[0]).each do |y|
    (0..max[1]).each do |x|
      print world[[x, y]]
    end
    puts
  end
  puts
end

def find_corners(world)
  m_x, m_y = world.keys.max_by { |c| c.reduce(:+) }
  [[0, 0], [m_x, 0], [0, m_y], [m_x, m_y]]
end

def next_step(world, santa_version)
  new_world = world.clone
  corners = find_corners(world)
  world.each do |(x, y), cell|
    adjacent_on =
      [[0, 1], [0, -1], [1, 0], [-1, 0], [1, 1], [1, -1], [-1, 1], [-1, -1]]
      .map { |d_x, d_y| world[[x + d_x, y + d_y]] }
      .compact
      .reduce(0) { |sum, c| sum + (c == '#' ? 1 : 0) }
    new_world[[x, y]] = '.'
    new_world[[x, y]] = '#' if cell == '#' && [2, 3].include?(adjacent_on)
    new_world[[x, y]] = '#' if cell == '.' && adjacent_on == 3
    new_world[[x, y]] = '#' if santa_version && corners.include?([x, y])
  end
  new_world
end

world_100_turns_part_one = 100.times.reduce(world) { |w, _| next_step(w, false) }
puts world_100_turns_part_one
  .values
  .reduce(0) { |sum, cell| sum + (cell == '#' ? 1 : 0) }


find_corners(world).each { |c| world[c] = '#' }

world_100_turns_part_two = 100.times.reduce(world) { |w, _| next_step(w, true) }
puts world_100_turns_part_two
  .values
  .reduce(0) { |sum, cell| sum + (cell == '#' ? 1 : 0) }
