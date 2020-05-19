module WriterTrans where
import Data.Monoid
import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Trans.Class

-- Writer w a -- тип вычислений с возвращаемым значением типа a
-- которые могут собирать какую-то информацию по ходу вычисления
newtype Writer w a = Writer { runWriter :: (a, w) }

-- он является монадой: return x возвращает значение, не записывая ничего в лог
-- а w >>= f применяет функцию f к значению действия w и комбинирует логи
instance Functor (Writer w) where
  fmap f (Writer (x, v)) = Writer (f x, v)
instance (Monoid w) => Applicative (Writer w) where
  pure x = Writer (x, mempty)
  Writer (f, v1) <*> Writer (x, v2) = Writer (f x, v1 <> v2)
instance (Monoid w) => Monad (Writer w) where  
  return = pure
  Writer (x,v) >>= f = let Writer (y, v') = f x in Writer (y, v <> v')

-- Некоторые полезные функции для Writer
-- writer (x, v) -- действие cо значением x и логом v
writer :: (a, w) -> Writer w a
writer (x, v) = Writer (x, v)
-- tell v -- действие со значением () и логом v
tell :: w -> Writer w ()
tell v = Writer ((), v)
-- listen w -- добавляет лог к значению действия
listen :: Writer w a -> Writer w (a, w)
listen (Writer (x, v)) = Writer ((x, v), v)
execWriter :: Writer w a -> w
execWriter = snd . runWriter

-- Трансформер монад, соответствующий Writer
-- При правильной реализации
-- WriterT w Identity эквивалентен Writer w 
-- writer' действует так же, как writer, при условии m == Identity
-- и т.д.
newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }

instance Functor (WriterT w m) where
  fmap = error "TODO"
instance (Monoid w) => Applicative (WriterT w m) where
  pure = error "TODO"
  (<*>) = error "TODO"
instance (Monoid w, Monad m) => Monad (WriterT w m) where
  return = pure
  (>>=) = error "TODO"

instance (Monoid w) => MonadTrans (WriterT w) where
  lift = error "TODO"

writer' :: Monad m => (a, w) -> WriterT w m a
writer' (x, v) = error "TODO"

tell' :: (Monad m, Monoid w) => w -> WriterT w m ()
tell' v = error "TODO"

listen' :: (Monad m, Monoid w) => WriterT w m a -> WriterT w m (a, w)
listen' mw = error "TODO"

execWriterT :: Monad m => WriterT w m a -> m w
execWriterT mw = error "TODO"
  
-- Дополнительное задание: реализовать StateT

-- Трансформер монад, соответствующий []
-- _После того, как его реализуете,_ прочитайте
-- http://www.haskell.org/haskellwiki/ListT_done_right
-- и посмотрите, удалось ли избежать описанных там ошибок
newtype ListT m a = TODO2 (m ())

instance MonadTrans ListT where
  lift = error "TODO"
