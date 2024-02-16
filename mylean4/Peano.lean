
-- https://live.lean-lang.org/

-- 定义自然数 ℕ（Nature Numbers）的归纳类型
inductive ℕ where            -- 依赖类型(dependent types)之归纳类型(inductive type) 定义自然数ℕ/Nature Numbers
  | zero : ℕ                 -- 构造子(constructor) zero
  | succ : ℕ -> ℕ           -- 构造子(constructor) succ/successor，定义为接受一个自然数作为参数，返回下一个自然数
  deriving Repr              --为该数据类型派生一种表示Repr/representation的方式，可能用于输出、打印或其他形式的显示

-- 将自定义的 ℕ 转换为 Lean 内置的 Nat 类型
def ℕ.toDefaultNat : ℕ -> Nat
  | ℕ.zero => 0
  | ℕ.succ n => 1 + ℕ.toDefaultNat n

-- 实例化 Repr 类型类，定义 ℕ 的显示方式
instance : Repr ℕ where
  reprPrec n _ := repr (ℕ.toDefaultNat n)

-- 示例：输出自然数 0、1、2 的显示方式
#eval ℕ.zero
#eval ℕ.succ ℕ.zero  
#eval ℕ.succ (ℕ.succ ℕ.zero)

-- 将 Lean 内置的 Nat 转换为自定义的 ℕ
def ℕ.fromDefaultNat : Nat -> ℕ
  | 0 => ℕ.zero
  | n + 1 => ℕ.succ (ℕ.fromDefaultNat n)

-- 实例化 OfNat 类型类，定义从 Nat 到 ℕ 的转换
instance : OfNat ℕ n where
  ofNat := ℕ.fromDefaultNat n

-- 定义 ℕ 的加法
def ℕ.add : ℕ -> ℕ -> ℕ
  | ℕ.zero, y => y
  | ℕ.succ x', y => ℕ.add x' (ℕ.succ y)

-- 示例：计算 0 + 1 和 2 + 3
#eval ℕ.add 0 1
#eval ℕ.add 2 3

-- 定义 ℕ 的减法
def ℕ.sub : ℕ -> ℕ -> ℕ
  | ℕ.succ x', ℕ.succ y' => ℕ.sub x' y'
  | x, _ => x

-- 示例：计算 1 - 0、3 - 2 和 4 - 5（注意最后一个例子可能出错）

-- 定义 ℕ 的乘法
def ℕ.mul : ℕ -> ℕ -> ℕ
  | x, ℕ.succ y' => ℕ.add x (ℕ.mul x y')
  | _, _ => ℕ.zero

-- 示例：计算 1 * 0 和 2 * 3

-- 定义 ℕ 的除法
def ℕ.div : ℕ -> ℕ -> ℕ
  | x, ℕ.succ y' => div' x y' 0 y'
  | _, ℕ.zero => ℕ.zero
  where
    div' : ℕ -> ℕ -> ℕ -> ℕ -> ℕ
      | ℕ.succ x', y, q, ℕ.zero => div' x' y (ℕ.succ q) y
      | ℕ.succ x', y, q, ℕ.succ r' => div' x' y q r'
      | ℕ.zero, _, q, _ => q

-- 示例：计算 0 / 1、1 / 0 和 2 / 4，注意最后两个例子可能出错

-- 定义 ℕ 的相等、大于、小于、大于等于、小于等于关系
def ℕ.eq : ℕ -> ℕ -> Bool
  | ℕ.zero, ℕ.zero => true
  | ℕ.succ x', ℕ.succ y' => ℕ.eq x' y'
  | _, _ => false

def ℕ.gt : ℕ -> ℕ -> Bool
  | ℕ.succ _, ℕ.zero => true 
  | ℕ.succ x', ℕ.succ y' => ℕ.gt x' y'
  | _, _ => false

def ℕ.lt : ℕ -> ℕ -> Bool
  | ℕ.zero, ℕ.succ _ => true
  | ℕ.succ x', ℕ.succ y' => ℕ.lt x' y'
  | _, _ => false

def ℕ.gte (x y : ℕ) : Bool := not (ℕ.lt x y)

def ℕ.lte (x y : ℕ) : Bool := not (ℕ.gt x y)

-- 示例：比较自然数的相等、大小关系

-- 定义自定义的运算符，使得代码更具可读性
infixl:70 " *' " => ℕ.mul
infixl:70 " /' " => ℕ.div
infixl:60 " +' " => ℕ.add
infixl:60 " -' " => ℕ.sub
infix:50 " ==' " => ℕ.eq        -- infix操作符后面的数字表示优先级
infix:50 " >' " => ℕ.gt
infix:50 " >=' " => ℕ.gte
infix:50 " <' " => ℕ.lt
infix:50 " <=' " => ℕ.lte

-- 示例：使用自定义运算符进行计算和比较
#eval 1 +' 1
#eval 1 ==' 1

