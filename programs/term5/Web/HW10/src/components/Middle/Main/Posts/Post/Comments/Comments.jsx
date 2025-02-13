import React, {useMemo} from 'react';
import Comment from "./Comment/Comment";

const Comments = ({comments}) => {
    const sortedComments = useMemo(() => {
        if (!comments)
            return []
        return comments.sort((a, b) => b.id - a.id)
    }, [comments])


    return (
        <div>
            {sortedComments.map((comment) => (
                <div key={comment.id}>
                    <Comment comment={comment} />
                </div>
            ))}
        </div>
    );
};

export default Comments;