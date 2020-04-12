input = File.readlines('input.txt')
          .map(&:to_i)

def combination_with_goal_weight(list, i, goal_weight)
  list.combination(i).to_a.select! { |l| l.sum == goal_weight }
end

def distribution_with_equal_weight(list, nr_of_groups, goal_weight)
  for i in 1..(list.length - 1)
    for g in combination_with_goal_weight(list, i, goal_weight)
      if nr_of_groups == 2
        return [g, list - g]
      else
        return [g].concat(distribution_with_equal_weight(list - g, nr_of_groups - 1, goal_weight))
      end
    end
  end

  []
end

def smalest_quantum_entanglement(list, nr_of_groups)
  goal_weight = list.sum / nr_of_groups
  for i in 1..(list.length - 1)
    result = combination_with_goal_weight(list, i, goal_weight)
               .map { |c| [c].concat(distribution_with_equal_weight(list - c, nr_of_groups - 1, goal_weight)) }
               .map { |x| x.first.reduce(&:*) }

    return result.min unless result.empty?
  end
end

p "Part 1: #{smalest_quantum_entanglement(input, 3)}"
p "Part 2: #{smalest_quantum_entanglement(input, 4)}"
