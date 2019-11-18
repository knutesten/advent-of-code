require 'set'

def reconstruct_path(came_from, current)
  path = [current]
  while came_from.key?(current)
    current = came_from[current]
    path.unshift(current)
  end
  path
end

def a_star(start, h, hard_mode = false)
  closed_set = Set[]
  came_from = {}
  g_score = { start => 0 }
  open_set = [start]

  f_score = { start => h.call(start) }

  until open_set.empty?
    current = open_set.sort_by! { |n| f_score[n] }.shift

    return reconstruct_path(came_from, current) if current.boss_hit_points <= 0

    closed_set << current

    find_neighbours(current, hard_mode).each do |neighbour|
      next if closed_set.include? neighbour

      cost = 0
      cost = current.spell_cast.cost unless current.spell_cast.nil?
      tentative_g_score = g_score[current] + cost

      if g_score[neighbour].nil? or tentative_g_score < g_score[neighbour]
        came_from[neighbour] = current
        g_score[neighbour] = tentative_g_score
        f_score[neighbour] = g_score[neighbour] + h.call(neighbour)
        open_set << neighbour unless open_set.include? neighbour
      end
    end
  end
end

Spell = Struct.new(:name, :cost, :damage, :armor, :healing, :mana_recharge, :duration)

$spells = [
  Spell.new("Magic Missile", 53, 4, 0, 0, 0, 0),
  Spell.new("Drain", 73, 2, 0, 2, 0, 0),
  Spell.new("Shield", 113, 0, 7, 0, 0, 6),
  Spell.new("Poison", 173, 3, 0, 0, 0, 6),
  Spell.new("Recharge", 229, 0, 0, 0, 101, 5)
]

State = Struct.new(:boss_hit_points, :boss_damage,
                   :player_hit_points, :player_armor, :player_mana,
                   :active_spells,
                   :mana_spent, :spell_cast, :turn) do
  def apply_spell(spell)
    self.boss_hit_points -= spell.damage
    self.player_hit_points += spell.healing
    self.player_mana += spell.mana_recharge
    self.player_armor = [spell.armor, self.player_armor].max
  end

  def deep_clone
    Marshal.load(Marshal.dump(self))
  end
end

def find_neighbours(state, hard_mode)
  next_state = state.deep_clone

  next_state.turn += 1
  next_state.spell_cast = nil
  next_state.player_armor = 0

  if hard_mode
    next_state.player_hit_points -= 1
    return [] if next_state.player_hit_points <= 0
  end

  next_state.active_spells.reject! { |spell|
    next_state.apply_spell(spell)
    spell.duration -= 1
    spell.duration == 0
  }

  return [next_state] if next_state.boss_hit_points <= 0

  if next_state.turn.odd?
    $spells
      .select { |spell| next_state.player_mana >= spell.cost }
      .select { |spell| !next_state.active_spells.map(&:name).include? spell.name }
      .map do |spell|
      state = next_state.deep_clone
      state.apply_spell(spell) if spell.duration == 0
      state.active_spells << spell if spell.duration > 0
      state.mana_spent += spell.cost
      state.player_mana -= spell.cost
      state.spell_cast = spell
      state
    end
  else
    next_state.player_hit_points -= [1, next_state.boss_damage - next_state.player_armor].max

    return [] if next_state.player_hit_points <= 0

    [next_state]
  end
end

boss_hit_points, boss_damage = File.readlines("input.txt").map { |l| l.split(": ")[1].to_i }
initial_state = State.new(boss_hit_points, boss_damage, 50, 0, 500, [], 0, nil, 0)

h = Proc.new { |s| s.boss_hit_points }
p a_star(initial_state, h).last.mana_spent
p a_star(initial_state, h, true).last.mana_spent

# path.each_cons(2).with_index do |(a, b), turn|
#   puts "-- Player turn --" if turn.even?
#   puts "-- Boss turn --" if turn.odd?
#   puts "-Player has #{a.player_hit_points} hit points, #{a.player_armor} armor, #{a.player_mana} mana"
#   puts "-Boss has #{a.boss_hit_points} hit points"
#   b.active_spells.each do |s|
#     puts "Spell #{s.name} is active; its timer is now #{s.duration}" if s != b.spell_cast
#   end
#   puts "Player casts #{b.spell_cast.name}" if turn.even?
#   puts "Boss attacks for #{b.boss_damage} - #{b.player_armor} = #{b.boss_damage - b.player_armor} damage!" if turn.odd?
#   puts
# end
