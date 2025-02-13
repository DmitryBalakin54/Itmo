import React, {useMemo} from 'react';
import Aside from "./Aside/Aside";

const Middle = ({posts, page}) => {

    const aside = useMemo(() => {
        return (<Aside posts={posts}/>)
    }, [posts])

    return (
        <div className="middle">
            {aside}
            <main>
                {page}
            </main>
        </div>
    );
};

export default Middle;