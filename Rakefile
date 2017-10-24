task :default => [:racket, :haskell]

task :racket do
  sh "raco test racket99.rkt"
end

task :r => :racket

task :haskell => :check do
  sh "runhaskell haskell99.hs"
end

task :check do
  sh "ghc -Werror -fno-code -Wunused-top-binds haskell99.hs"
end

task :h => :haskell
