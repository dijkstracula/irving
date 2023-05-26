
pub type VisitorResult<E> = std::result::Result<Control, E>;

pub enum Control {
    // Proceed with the post-order traversal
    Continue,

    // Remove the current node entirely (this only makes
    // sense if the node is in a compound block)
    Remove
}
