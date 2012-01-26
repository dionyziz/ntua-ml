function [fea2] = tfidf1(fea)

[nSmp,nFea] = size(fea);
[idx,jdx,vv] = find(fea);

df = full(sum(sparse(idx,jdx,1),1));
idf = log(nSmp./df);

for i=1:nSmp
tf(i,:)=sparse(fea(i,:)/sum(fea(i,:))); % max or sum
end

for j=1:nFea
fea2(:,j)=sparse(tf(:,j) * idf (j));
end

fea2=sparse(fea2);

end