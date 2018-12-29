%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Problem 3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%(a)
%Reading the three text files.
fid = fopen('senate_names.txt','r');
senateNames = textscan(fid, '%s',100,'Delimiter','\n');
fclose(fid);

%load command used as the files conatin numeric content
senateParties = load('senate_parties.txt');
senateVotes = load('senate_votes.txt');

%Segragating the files to load indices of the parties into variables
%according to the party number i.e. 100 or 200 or other
senatePartiesDemocrats = find(senateParties == 100);
senatePartiesRepublicans = find(senateParties == 200);
senatePartiesIndependent = find(senateParties ~= 100 & senateParties ~= 200); %19

%(b)
%Computing the SVD values
[USenateVoting,SSenateVoting,VSenateVoting] = svd(senateVotes);

figure();
%Plotting
plot(diag(SSenateVoting))
xlabel('')
ylabel('')
title('Plotting the singular values')
legend('')
%Inference - The singular values are maximum in the beginning till they gradually turn
%into comparable values(almost identical) after the 10th value.

%(c)
figure()
U1 = USenateVoting(:,1)
U2 = USenateVoting(:,2)

%Setting color codes to differentiate the data points of the 3 different
%classes - Democrats as yellow, Republicans as magenta and Independent as
%Cyan
color=ones(100,3);
yellow = [1 1 0]
magenta = [1 0 1]

color(senatePartiesDemocrats,:)=repmat(yellow,[length(senatePartiesDemocrats) 1]);
color(senatePartiesRepublicans,:)=repmat(magenta,[length(senatePartiesRepublicans) 1]); 
color(senatePartiesIndependent,:)=[0 1 1];

%Plot
scatter(U1,U2,[],color,'filled')
xlabel('U1')
ylabel('U2')
title('Scatter plot of the first and second columns of U')
legend('Republicans','Democrats','Independant')

%(d)
%The first column of U (U1) shows a higher variance as compared to the second
%column i.e. U1
figure()
plot(U1,':')
hold on 
plot(U2)
hold off

%(e)
senateVotingAppr = USenateVoting(:,[1,2]) * SSenateVoting(1:2,1:2) * VSenateVoting(:,1:2)'

yea = senateVotingAppr;
nay = senateVotingAppr;

%For values greater than 0 is considered positive set as 1 and the ones
%lesser than 0 are negative values set to -1
yea(senateVotingAppr<=0)=0;
nay(senateVotingAppr>=0)=0;
yea(senateVotingAppr>0)=1;
nay(senateVotingAppr<0)=-1;

senateVotingFinal = yea+nay

%Finding the correct predictions
correctSenate = senateVotes==senateVotingFinal

%Total correct predictions
sum(sum(correctSenate))

sizeOfSenateVotes = size(senateVotes)

%Based on low rank approximations, finding the best p < rank value as a
%computation of the fraction of the correct predication amoung the total
%readings
%which is 86.06 percent
predRateSenate = sum(sum(correctSenate))/(sizeOfSenateVotes(1) * sizeOfSenateVotes(2))

%Plotting a scatter plot of each of the row sums vs U1
figure()
scatter(sum(correctSenate,2)/sizeOfSenateVotes(2),U1,100,color,'filled')
xlabel('Accuracy')
ylabel('U1')
title('Scatter plot of the first and second columns of U')
legend('Republicans','Democrats','Independant')
%text(sum(correctSenate,2)/sizeOfSenateVotes(2),U1,senateNames{1})

%(f)
%Same steps followed for house data
fID = fopen('house_names.txt','r');
houseNames = textscan(fID, '%s',403,'Delimiter','\n');
fclose(fID);
disp(houseNames{1});

houseParties = load('house_parties.txt');
houseVotes = load('house_votes.txt');

housePartiesDemocrats = find(houseParties == 100);
housePartiesRepublicans = find(houseParties == 200);
housePartiesIndependent = find(and(houseParties ~= 100,houseParties ~= 200)); %19


[UHouseVoting,SHouseVoting,VHouseVoting] = svd(houseVotes);

length(diag(SHouseVoting));
figure();
%Inference - The singular values are maximum in the beginning till they gradually turn
%into comparable values(almost identical) after the 40th value.
plot(diag(SHouseVoting));


figure()

UH1 = UHouseVoting(:,1)
UH2 = UHouseVoting(:,2)

color=ones(403,3);
yellow = [1 1 0]
magenta = [1 0 1]
cyan = [0 1 1]

color(housePartiesDemocrats,:)=repmat(yellow,[length(housePartiesDemocrats) 1]);
color(housePartiesRepublicans,:)=repmat(magenta,[length(housePartiesRepublicans) 1]); 
color(housePartiesIndependent,:)= repmat(cyan,[length(housePartiesIndependent) 1]); 
scatter(UH1,UH2,[],color,'filled')


%Maximum variance shown in first column
figure()
plot(UHouseVoting(:,1))
%Second maximum variance shown in the second column of UHouseVoting
figure()
plot(UHouseVoting(:,2))

houseVotingAppr = UHouseVoting(:,[1,2]) * SHouseVoting(1:2,1:2) * VHouseVoting(:,1:2)'
%For values greater than 0 is considered positive set as 1 and the ones
%lesser than 0 are negative values set to -1
yeaHouse = houseVotingAppr;
nayHouse = houseVotingAppr;
yeaHouse(houseVotingAppr<0)=0;
nayHouse(houseVotingAppr>0)=0;

yeaHouse(houseVotingAppr>0)=1;
nayHouse(houseVotingAppr<0)=-1;

%Adding values to formulate a matrix to contain all votes
houseVotingFinal = yeaHouse+nayHouse


sizeOfHouseVotes = size(houseVotes)
%Checking the correct predictions which is 554515
correctHouse = houseVotes==houseVotingFinal 

%Correct prediction
sum(sum(correctHouse))

%Based on low rank approximations, finding the best p < rank value as a
%computation of the fraction of the correct prediction amoung the total
%readings
%which is 85.89 percent
predRateHouse = sum(sum(correctHouse))/(sizeOfHouseVotes(1) * sizeOfHouseVotes(2))

%Plotting a scatter plot of each of the row sums vs U1figure()
plot(sum(houseVotes,2))
text(1:sizeOfHouseVotes(1),sum(houseVotes,2),houseNames{1})
figure()
scatter(sum(correctHouse,2)/sizeOfHouseVotes(2),UHouseVoting(:,1),100,color,'filled')
text(sum(correctHouse,2)/sizeOfHouseVotes(2),UH1,houseNames{1})
