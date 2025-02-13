import React, {useMemo} from 'react';
import Post from "./Post/Post";

const Posts = ({user, posts, setPage, setPageArgs, createComment}) => {
    const sortedPosts = useMemo(() => {
        if (!posts)
            return []
        return posts.sort((a, b) => b.id - a.id)
    }, [posts])

    return (
        <div>
            {sortedPosts.map((post) => (
                <Post post={post} setPage={setPage} setPageArgs={setPageArgs} viewComments={false} user={user}
                      createComment={createComment}/>
            ))}
        </div>
    );
};

export default Posts;