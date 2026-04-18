/// Convert a WOFF1 binary to raw SFNT/OpenType bytes that fontdb can load.
/// WOFF1 wraps each OpenType table in an optional zlib deflate stream; this
/// decompresses each table and reconstructs a standard SFNT binary.
pub fn woff1_to_sfnt(woff: &[u8]) -> Option<Vec<u8>> {
    use std::convert::TryInto;

    if woff.len() < 44 { return None; }
    if &woff[0..4] != b"wOFF" { return None; }

    let sfnt_flavor = u32::from_be_bytes(woff[4..8].try_into().ok()?);
    let num_tables = u16::from_be_bytes(woff[12..14].try_into().ok()?) as usize;

    if woff.len() < 44 + num_tables * 20 { return None; }

    struct Entry { tag: [u8; 4], woff_off: usize, comp_len: usize, orig_len: usize, checksum: u32 }
    let mut tables: Vec<Entry> = Vec::with_capacity(num_tables);
    for i in 0..num_tables {
        let e = 44 + i * 20;
        tables.push(Entry {
            tag:      woff[e..e+4].try_into().ok()?,
            woff_off: u32::from_be_bytes(woff[e+4..e+8].try_into().ok()?)  as usize,
            comp_len: u32::from_be_bytes(woff[e+8..e+12].try_into().ok()?) as usize,
            orig_len: u32::from_be_bytes(woff[e+12..e+16].try_into().ok()?) as usize,
            checksum: u32::from_be_bytes(woff[e+16..e+20].try_into().ok()?),
        });
    }
    tables.sort_by(|a, b| a.tag.cmp(&b.tag));

    let n = num_tables as u16;
    let mut x = 1u16;
    while x * 2 <= n { x *= 2; }
    let search_range   = x * 16;
    let entry_selector = (x as f32).log2() as u16;
    let range_shift    = n * 16 - search_range;

    let header_size = 12 + num_tables as u32 * 16;
    let mut sfnt_offsets: Vec<u32> = Vec::with_capacity(num_tables);
    let mut cursor = header_size;
    for t in &tables {
        sfnt_offsets.push(cursor);
        cursor += t.orig_len as u32;
        cursor = (cursor + 3) & !3;
    }

    let mut out = vec![0u8; cursor as usize];

    out[0..4].copy_from_slice(&sfnt_flavor.to_be_bytes());
    out[4..6].copy_from_slice(&n.to_be_bytes());
    out[6..8].copy_from_slice(&search_range.to_be_bytes());
    out[8..10].copy_from_slice(&entry_selector.to_be_bytes());
    out[10..12].copy_from_slice(&range_shift.to_be_bytes());

    for (i, (t, &off)) in tables.iter().zip(sfnt_offsets.iter()).enumerate() {
        let d = 12 + i * 16;
        out[d..d+4].copy_from_slice(&t.tag);
        out[d+4..d+8].copy_from_slice(&t.checksum.to_be_bytes());
        out[d+8..d+12].copy_from_slice(&off.to_be_bytes());
        out[d+12..d+16].copy_from_slice(&(t.orig_len as u32).to_be_bytes());

        let src = woff.get(t.woff_off..t.woff_off + t.comp_len)?;
        let data: Vec<u8> = if t.comp_len < t.orig_len {
            miniz_oxide::inflate::decompress_to_vec_zlib(src).ok()?
        } else {
            src.to_vec()
        };
        let start = off as usize;
        out[start..start + data.len()].copy_from_slice(&data);
    }
    Some(out)
}
