import React from 'react';

const Comment = ({comment}) => {
    return (
        <article>
            <div className="author">Author: {comment.author}</div>
            <div className="body" style={{whiteSpace: "pre-line"}}>{comment.text}</div>
        </article>
    );
};

export default Comment;