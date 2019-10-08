weapons = [
  [8, 4, 0],
  [10, 5, 0],
  [25, 6, 0],
  [40, 7, 0],
  [74, 8, 0]
]

armors = [
  [0, 0, 0],
  [13, 0, 1],
  [31, 0, 2],
  [53, 0, 3],
  [75, 0, 4],
  [102, 0, 5]
]

rings = [
  [0, 0, 0],
  [0, 0, 0],
  [25, 1, 0],
  [50, 2, 0],
  [100, 3, 0],
  [20, 0, 1],
  [40, 0, 2],
  [80, 0, 3]
]

Player = Struct.new(:hit_points, :damage, :armor)
boss = Player.new(*File
                     .readlines("input.txt")
                     .map { |l| l.split(": ")[1].to_i })

def fight(player, boss)
  player_damage = [player.damage - boss.armor, 1].max
  boss_damage = [boss.damage - player.armor, 1].max

  (boss.hit_points.to_f / player_damage).ceil <= (player.hit_points.to_f / boss_damage).ceil
end

min_cost = 100000
max_cost = 0
weapons.each do |w|
  armors.each do |a|
    rings.combination(2).each do |r|
      cost, damage, armor = (r + [w, a]).transpose.map { |x| x.reduce(:+) }
      player = Player.new(100, damage, armor)

      min_cost = cost if cost < min_cost and fight(player, boss)
      max_cost = cost if cost > max_cost and !fight(player, boss)
    end
  end
end

puts min_cost
puts max_cost




