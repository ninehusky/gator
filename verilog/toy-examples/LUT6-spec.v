`timescale  1 ps / 1 ps
// @ninehusky: This is my bomb ass program

module LUT6
(
    input  wire I0, I1, I2, I3, I4, I5,
    output wire O
);
    assign O = I0 * I1 * I2 * I3 * I4 * I5;

endmodule
