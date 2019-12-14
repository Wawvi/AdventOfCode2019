#as of day 11

function interprete_intruction(instruction)
    Dict(
        "op_code" => instruction % 100,
        "mode_param_a" => instruction % 1000 ÷ 100,
        "mode_param_b" => instruction % 10000 ÷ 1000,
        "mode_param_c" => instruction % 100000 ÷ 10000
    )
end

function parameter_mode(program, param_mode, param_value, relative_base)
    if param_mode == 0
        program[param_value + 1]
    elseif param_mode == 1
        param_value
    elseif param_mode == 2
        program[param_value + 1 + relative_base]
    else
        error("Parameter mode out of bond")
    end
end

function program_index_test(input, index)
    """function to append 0s if position is out of bond, else return position"""
    if index > length(input)
        append!(input, zeros(Int64, index-length(input)) )
    end
end

function intcode_computer(program, input = 0, phase_setting = input, position = 1, output = 0, relative_base = 0, one_shot = false)

    input = [input, phase_setting]

    phase_used = position != 1

    output = Dict(
        "output" => Int64[],
        "position" => position,
        "program" => program,
        "stop_99" => false,
        "phase" => phase_setting,
        "relative_base" => relative_base
    )

    while true
        program_index_test(program, position)
        instruction = interprete_intruction(program[position])

        #absolute value
        value_a = try program[position + 1] catch e 0 end
        value_b = try program[position + 2] catch e 0 end
        value_c = try program[position + 3] catch e 0 end

        #parameter value after mode
        param_a = try parameter_mode(program, instruction["mode_param_a"], value_a, relative_base) catch e 0 end
        param_b = try parameter_mode(program, instruction["mode_param_b"], value_b, relative_base) catch e 0 end
        param_c = try parameter_mode(program, instruction["mode_param_c"], value_c, relative_base) catch e 0 end

        #pointer position for op_code modifications
        position_a = instruction["mode_param_a"] == 2 ? (value_a + relative_base + 1) : value_a + 1
        position_b = instruction["mode_param_b"] == 2 ? (value_b + relative_base + 1) : value_b + 1
        position_c = instruction["mode_param_c"] == 2 ? (value_c + relative_base + 1) : value_c + 1

        #op_code instructions

        if instruction["op_code"] == 1
            program_index_test(program, position_c)
            program[position_c] = param_a + param_b
            position += 4

        elseif instruction["op_code"] == 2
            program_index_test(program, position_c)
            program[position_c] = param_a * param_b
            position += 4

        elseif instruction["op_code"] == 3
            program_index_test(program, position_a)
            program[position_a] = phase_used ? input[2] : input[1]
            position += 2

        elseif instruction["op_code"] == 4
            position += 2
            push!(output["output"], param_a)
            output["position"] = position
            output["program"] = program
            output["relative_base"] = relative_base
            if one_shot return output end

        elseif instruction["op_code"] == 5
            param_a != 0 ? position = param_b + 1 : position += 3

        elseif instruction["op_code"] == 6
            param_a == 0 ? position = param_b + 1 : position += 3

        elseif instruction["op_code"] == 7
            program_index_test(program, position_c)
            program[position_c] = param_a < param_b ? 1 : 0
            position += 4

        elseif instruction["op_code"] == 8
            program_index_test(program, position_c)
            program[position_c] = param_a == param_b ? 1 : 0
            position += 4

        elseif instruction["op_code"] == 9
            relative_base += param_a
            position += 2

        elseif instruction["op_code"] == 99
            output["position"] = position
            output["program"] = program
            output["relative_base"] = relative_base
            output["stop_99"] = true
            return output

        else
            error("Error in op_code !")
        end
    end

end
