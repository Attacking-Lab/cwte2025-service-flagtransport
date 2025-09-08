for x in "$(dirname "$0")/../service/src/"*; do
    mv "$x" "$x.orig"
    python3 fill_sequence_number_area.py "$x.orig" > "$x"
    rm "$x.orig"
done
