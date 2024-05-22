module muladd_1_cycle_sketch(
    input [3:0] a,
    input [3:0] b,
    input [3:0] c,
    input clk,
    input USE_REG, // if this is on, use a register. otherwise, it is combinational.
    output [3:0] out
);
    reg [3:0] store;
    always @(posedge clk) begin
        if (USE_REG) begin
            store <= a * b + c;
        end
    end
    assign out = USE_REG ? store : a * b + c;
endmodule