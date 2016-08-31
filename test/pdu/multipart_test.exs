defmodule SMPPEX.Pdu.MultipartTest do
  alias SMPPEX.Pdu.Multipart
  alias SMPPEX.Pdu

  use ExUnit.Case

  doctest Multipart

  test "multipart_ie" do
    assert {:error, _} = Multipart.multipart_ie({-1, 1, 1})
    assert {:error, _} = Multipart.multipart_ie({65536, 1, 1})
    assert {:error, _} = Multipart.multipart_ie({1, 0, 1})
    assert {:error, _} = Multipart.multipart_ie({1, 256, 1})
    assert {:error, _} = Multipart.multipart_ie({1, 1, 0})
  end

  test "split_message" do
    assert {:ok, :unsplit} == Multipart.split_message(123, "", 0)
    assert {:ok, :split, [
        <<0x05, 0x00, 0x03, 0x7b, 0x08, 0x01, "a">>,
        <<0x05, 0x00, 0x03, 0x7b, 0x08, 0x02, "b">>,
        <<0x05, 0x00, 0x03, 0x7b, 0x08, 0x03, "c">>,
        <<0x05, 0x00, 0x03, 0x7b, 0x08, 0x04, "d">>,
        <<0x05, 0x00, 0x03, 0x7b, 0x08, 0x05, "e">>,
        <<0x05, 0x00, 0x03, 0x7b, 0x08, 0x06, "f">>,
        <<0x05, 0x00, 0x03, 0x7b, 0x08, 0x07, "g">>,
        <<0x05, 0x00, 0x03, 0x7b, 0x08, 0x08, "h">>
      ]} == Multipart.split_message(123, "abcdefgh", 7)

    assert {:error, _} = Multipart.split_message(123, "abcdefg", 6, 0)
  end

end

