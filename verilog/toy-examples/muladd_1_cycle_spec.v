module muladd_1_cycle_spec(
    input [1:0] a,
    input [1:0] b,
    input [1:0] c,
    input clk,
    output [3:0] out
);
    reg [3:0] store;
    always @(posedge clk) begin
        store <= a * b + c;
    end
    assign out = store;
endmodule