require 'set'

lines = File
          .readlines("./input.txt")
          .map(&:strip)

start_molecule = lines.pop
$replacements = lines[0..-2]
                  .each_with_object([]) { |line, acc| acc << line.split(" => ") }

def find_neighbours(molecule)
  $replacements.each_with_object(Set[]) do |replacement, acc|
    from, to = replacement
    index = molecule.index(from)
    until index.nil?
      new_to = molecule[0, index] + to + molecule[(index + from.size), molecule.size]
      acc << new_to
      index = molecule.index(from, index + 1)
    end
  end
end

puts find_neighbours(start_molecule).size

def reconstruct_path(came_from, current)
  path = [current]
  while came_from.key?(current)
    current = came_from[current]
    path.unshift(current)
  end
  path
end

def a_star(start, goal, h)
  closed_set = Set[]
  came_from = {}
  g_score = { start => 0 }
  open_set = [start]

  f_score = { start => h.call(start) }

  until open_set.empty?
    current = open_set.sort_by! { |n| f_score[n] }.shift

    return reconstruct_path(came_from, current) if goal == current

    closed_set << current

    find_neighbours(current).each do |neighbour|
      next if closed_set.include? neighbour

      tentative_g_score = g_score[current] + 1

      if g_score[neighbour].nil? or tentative_g_score < g_score[neighbour]
        came_from[neighbour] = current
        g_score[neighbour] = tentative_g_score
        f_score[neighbour] = g_score[neighbour] + h.call(neighbour)
        open_set << neighbour unless open_set.include? neighbour
      end
    end
  end
end

$replacements = $replacements.map(&:reverse)
puts a_star(start_molecule, "e", proc(&:size)).size - 1
