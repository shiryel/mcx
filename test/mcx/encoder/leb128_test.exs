defmodule Mcx.Encoder.LEB128Test do
  use ExUnit.Case, async: true

  alias Mcx.Encoder.LEB128

  describe "decode/1" do
    test "0" do
      {0, ""} = LEB128.decode(0x00)
    end

    test "1" do
      {1, ""} = LEB128.decode(0x01)
    end
  end
end
