pub type VisitorResult<T, E> = std::result::Result<Control<T>, E>;

pub enum Control<T> {
    // Proceed with the post-order traversal, returning some value
    Continue(T),

    // Remove the current node entirely (this only makes
    // sense if the node is in a compound block)
    Remove,
}
