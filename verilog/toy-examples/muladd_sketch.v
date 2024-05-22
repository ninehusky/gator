module muladd(
    input [3:0] a,
    input [3:0] b,
    input [3:0] SASHAS_INPUT,
    output [3:0] out
);
    assign out = (a * b) + SASHAS_INPUT;
endmodule