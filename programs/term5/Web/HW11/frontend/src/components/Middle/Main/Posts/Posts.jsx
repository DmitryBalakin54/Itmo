import React, {useEffect, useMemo, useState} from 'react';
import Post from "./Post/Post";
import axios from "axios";

const Posts = () => {

    const [allPosts, setAllPosts] = useState(null)


    useEffect(() => {
        axios.get("/api/posts").then((response)=>{
            setAllPosts(response.data)
        }).catch((error)=>{
            console.log(error)
        })
    }, []);

    const sortedPosts = useMemo(() => {
        if (!allPosts)
            return []
        return allPosts.sort((a, b) => b.creationTime - a.creationTime)
    }, [allPosts])

    return (
        <div>
            {sortedPosts.map((post) => (
                <Post post={post} key={post.id} isOnlyOne={false} />
            ))}
        </div>
    );
};

export default Posts;