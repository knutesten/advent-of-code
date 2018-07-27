Aunt = Struct.new(:id, :children, :cats, :samoyeds, :pomeranians,
                  :akitas, :vizslas, :goldfish, :trees, :cars, :perfumes) do
  def match_part_one?(props)
    props.all? { |key, value| self[key].nil? || self[key] == value }
  end

  def match_part_two?(props)
    props.all? do |key, value|
      self[key].nil? ||
        case key
        when :cats, :trees           then self[key] > value
        when :pomeranians, :goldfish then self[key] < value
        else                              self[key] == value
        end
    end
  end
end

aunts = File
        .readlines('./input.txt')
        .reduce([]) do |list, line|
  id, prop1, prop2, prop3 = line.match(/(\d+): (.*), (.*), (.*)$/).captures

  args = [prop1, prop2, prop3].each_with_object(id: id) do |prop, hash|
    key, value = prop.split(': ')
    hash[key.to_sym] = value.to_i
  end

  list << Aunt.new(*args.values_at(*Aunt.members))
end

mfcsam_output = {
  children: 3,
  cats: 7,
  samoyeds: 2,
  pomeranians: 3,
  akitas: 0,
  vizslas: 0,
  goldfish: 5,
  trees: 3,
  cars: 2,
  perfumes: 1
}

puts aunts.select { |aunt| aunt.match_part_one?(mfcsam_output) }.first[:id]
puts aunts.select { |aunt| aunt.match_part_two?(mfcsam_output) }.first[:id]
