#Day 1

## Input
input = readlines("input1.txt")
input = parse.(Int64, input)

## Part 1

fuel_req(mass) = (mass ÷ 3 - 2)
sum([fuel_req(mass) for mass in input])

## Part 2
fuel_req(mass) = (mass ÷ 3 - 2) >= 0 ? (mass ÷ 3 - 2) : 0
function moduel_fuel_req(mass)
    list = Int64[]
    while fuel_req(mass) > 0
        mass = fuel_req(mass)
        push!(list, mass)
    end
    return sum(list)
end
sum([moduel_fuel_req(mass) for mass in input])
