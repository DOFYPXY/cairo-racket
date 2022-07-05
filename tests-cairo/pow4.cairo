func pow4(n) -> (m : felt):
    alloc_locals
    local x

    jmp body if n != 0
    [ap] = 0; ap++
    ret

    body:
    x = n * n
    [ap] = x * x; ap++
    ret
end

func main():
    pow4(n=5)
    ret
end