task :default => [:racket, :haskell]

task :racket do
  sh "raco test racket99.rkt"
end

task :r => :racket

task :haskell do
  sh "TERM=dumb runhaskell haskell99.hs < /dev/null"
end

task :h => :haskell
