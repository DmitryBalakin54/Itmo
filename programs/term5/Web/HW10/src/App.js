import './App.css';
import Enter from "./components/Middle/Main/Enter/Enter";
import WritePost from "./components/Middle/Main/WritePost/WritePost";
import Index from "./components/Middle/Main/Index/Index";
import React, {useCallback, useState} from "react";
import Middle from "./components/Middle/Middle";
import Footer from "./components/Footer/Footer";
import Header from "./components/Header/Header";
import Register from "./components/Middle/Main/Register/Register";
import AllUsers from "./components/Middle/Main/AllUsers/AllUsers";
import Posts from "./components/Middle/Main/Posts/Posts";
import Post from "./components/Middle/Main/Posts/Post/Post";
import comments from "./components/Middle/Main/Posts/Post/Comments/Comments";

function App({usersData, postsData}) {

    const [user, setUser] = useState(null)
    const [users, setNewUser] = useState(usersData)
    const [page, setPage] = useState('index')
    const [pageArgs, setPageArgs] = useState({})
    const [posts, setPosts] = useState(postsData)

    const createPost = useCallback((post) => {
        setPosts((prevPosts) => {
            const maxId = Math.max(0, ...prevPosts.map((p) => p.id)) + 1;
            return [...prevPosts, {...post, id: maxId}];
        });
    }, [posts]);

    const createUser = useCallback((user) => {
        setNewUser((prevUsers) => {
            const maxId = Math.max(0, ...prevUsers.map((u) => u.id)) + 1;
            return [...prevUsers, {...user, id: maxId}];
        });
    }, [users]);

    const createComment = useCallback((comment) => {
        setPosts((prevPosts) => {
            return prevPosts.map((post) => {
                if (post.id === comment.postId) {
                    const coms = post.comments ? post.comments : []
                    const maxCommentId = Math.max(0, ...coms.map((c) => c.id)) + 1;
                    return {
                        ...post,
                        comments: [...coms, {...comment, id: maxCommentId}],
                    };
                }
                return post;
            });
        });
    }, [posts]);

    const getPage = useCallback((page) => {
        switch (page) {
            case 'index':
                return (<Index/>)
            case 'enter':
                return (<Enter users={users} setUser={setUser} setPage={setPage}/>)
            case 'writePost':
                return (<WritePost createPost={createPost} setPage={setPage}/>)
            case 'register':
                return (<Register users={users} createUser={createUser} setPage={setPage}/>)
            case 'allUsers':
                return <AllUsers users={users}/>
            case 'posts':
                return (<Posts user={user} posts={posts} setPage={setPage} setPageArgs={setPageArgs}
                               createComment={createComment}/>)
            case 'post':
                const currentPost = posts.find((post) => post.id === pageArgs.post.id);
                return (<Post user={user} setPage={setPage} setPageArgs={setPageArgs} post={currentPost}
                              viewComments={pageArgs.viewComments} createComment={createComment}/>)
        }
    }, [users, posts, pageArgs, page, setPosts])

    return (
        <div className="App">
            <Header setUser={setUser} setPage={setPage} user={user}/>
            <Middle
                posts={posts}
                page={getPage(page)}
                setPage={setPage}
                setPageArgs={setPageArgs}
            />
            <Footer usersCount={users.length} postsCount={posts.length}/>
        </div>
    );
}

export default App;
