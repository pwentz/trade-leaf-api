module MatchFinder where

import Config (App)
import Database.Esqueleto
import qualified Database.Persist.Sql as Sql
import Models

{-|
  select offer.* from
    (select request.*
     from offer
     inner join public.user on public.user.id = offer.user_id
     inner join request on request.category_id = offer.category_id
     where public.user.username = 'pat'
    ) as myReqs
  inner join offer on offer.id = myReqs.offer_id;
-}
findMatches :: User -> App [Sql.Entity Offer]
findMatches user =
    let userReqs user =
            from $ \(requests `InnerJoin` offers `InnerJoin` users) -> do
                on (offers ^. OfferUserId ==. users ^. UserId)
                on (requests ^. RequestCategoryId ==. offers ^. OfferCategoryId)
                where_ (users ^. UserUsername ==. val (userUsername user))
                return requests
    in runDb $
       select $
       from $ \offers -> do
           userReq <- userReqs user
           where_ (offers ^. OfferId ==. userReq ^. RequestOfferId)
           return offers
