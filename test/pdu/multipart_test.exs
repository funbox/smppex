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
    assert {:error, _} = Multipart.multipart_ie({1, 1, 256})

    assert {:ok, {0, <<0x03, 0x02, 0x01>>}} = Multipart.multipart_ie({3,2,1})
    assert {:ok, {8, <<0x01, 0x00, 0x02, 0x01>>}} = Multipart.multipart_ie({256,2,1})
  end

  test "prepend_message_with_part_info" do
    data = <<0x05, 0x00, 0x03, 0x03, 0x02, 0x01, "message">>
    assert {:ok, data} == Multipart.prepend_message_with_part_info({3,2,1}, "message")

    data = <<0x06, 0x08, 0x04, 0x01, 0x00, 0x02, 0x01, "message">>
    assert {:ok, data} == Multipart.prepend_message_with_part_info({256,2,1}, "message")
  end

  test "split_message" do
    assert {:ok, :unsplit} == Multipart.split_message(123, "", 0)
    assert {:ok, :unsplit} == Multipart.split_message(123, "abc", 3)
    assert {:error, _} = Multipart.split_message(123, "abcdefg", 6)
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

    assert {:ok, :split, [
        <<0x05, 0x00, 0x03, 0x7b, 0x05, 0x01, "ab">>,
        <<0x05, 0x00, 0x03, 0x7b, 0x05, 0x02, "cd">>,
        <<0x05, 0x00, 0x03, 0x7b, 0x05, 0x03, "ef">>,
        <<0x05, 0x00, 0x03, 0x7b, 0x05, 0x04, "gh">>,
        <<0x05, 0x00, 0x03, 0x7b, 0x05, 0x05, "i">>,
      ]} == Multipart.split_message(123, "abcdefghi", 8)

    assert {:ok, :split, [
        <<0x05, 0x00, 0x03, 0x7b, 0x05, 0x01, "ab">>,
        <<0x05, 0x00, 0x03, 0x7b, 0x05, 0x02, "cd">>,
        <<0x05, 0x00, 0x03, 0x7b, 0x05, 0x03, "ef">>,
        <<0x05, 0x00, 0x03, 0x7b, 0x05, 0x04, "gh">>,
        <<0x05, 0x00, 0x03, 0x7b, 0x05, 0x05, "i">>,
      ]} == Multipart.split_message(123, "abcdefghi", 0, 2)

    assert {:error, _} = Multipart.split_message(123, "abcdefg", 6, 0)
  end

end

