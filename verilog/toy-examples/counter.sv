module counter(
    input wire clk,
    output reg [3:0] count
);
    always @(posedge clk) begin
        count <= count + 1;
    end
endmodule