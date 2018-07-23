require 'digest'


def find_postfix_number(secret_key, starts_with)
  md5 = Digest::MD5.new
  hash = ''
  postfix = 0
  until hash.start_with?(starts_with)
    postfix += 1
    md5.reset
    md5 << secret_key + postfix.to_s
    hash = md5.hexdigest
  end

  postfix
end

secret_key = 'ckczppom'
puts find_postfix_number(secret_key, '00000')
puts find_postfix_number(secret_key, '000000')


