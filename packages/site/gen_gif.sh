#! /usr/bin/env nix-shell
#! nix-shell -i bash -p imagemagick

if [ "$#" -lt 3 ]; then
    echo "Usage: $0 <outdir> <spritesheet> <delay>"
    echo "Example: $0 output/ bee.png 20"
    exit 1
fi

OUTDIR="$1"
SPRITESHEET="$2"
DELAY="$3"

# Create output directory if it doesn't exist
mkdir -p "$OUTDIR"

# Get base filename without extension
BASENAME=$(basename "$SPRITESHEET" .png)

# Create temporary directory for frames
TMPDIR=$(mktemp -d)

# Split into frames
magick "$SPRITESHEET" -crop 3x4@ +repage +adjoin "$TMPDIR/frame_%d.png"

# Create directional animations
magick -dispose background -delay "$DELAY" "$TMPDIR/frame_[0-2].png" "$OUTDIR/${BASENAME}_south.gif"
magick -dispose background -delay "$DELAY" "$TMPDIR/frame_[3-5].png" "$OUTDIR/${BASENAME}_west.gif"
magick -dispose background -delay "$DELAY" "$TMPDIR/frame_[6-8].png" "$OUTDIR/${BASENAME}_east.gif"
magick -dispose background -delay "$DELAY" "$TMPDIR/frame_[9-11].png" "$OUTDIR/${BASENAME}_north.gif"

# Cleanup
rm -rf "$TMPDIR"

echo "Created animations in $OUTDIR:"
ls -l "$OUTDIR/${BASENAME}"*.gif
