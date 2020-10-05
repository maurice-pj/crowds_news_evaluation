function shuffleArray(l) {
	/*Randomize  in-place using the Durstenfield shuffle algorithm*/
	for (var i = l.length - 1; i > 0; i--) {
        var j = Math.floor(Math.random() * (i + 1));
        var temp = l[i];
        l[i] = l[j];
        l[j] = temp;
    }
    return l;
}

Qualtrics.SurveyEngine.addOnload(function()
{
	/*Place your JavaScript here to run when the page loads*/

});

Qualtrics.SurveyEngine.addOnReady(function()
{
	/*Place your JavaScript here to run when the page is fully displayed*/
	/*Initialize headlines*/
		dem_headlines = [
			[["Dem_1"], ["Justice Dept. Officials Had Discussions About Pushing Trump Out"]],
			[["Dem_2"], ["Undeniable Warming: The Planet's Hottest Five Years on Record"]],
			[["Dem_3"], ["Trump Says South Korea is Paying $500 Million More for U.S. Troops. The Deal Says Otherwise."]],
			[["Dem_4"], ["Since Parkland There's Been a School Shooting, on Average, Every 12 Days"]],
			[["Dem_5"], ["Demand for IUDs and Birth Control Implants Rose After Trump's Election Amid Insurance Concerns"]],
			[["Dem_6"], ["Justice Department to Award $8.3 Million to California Victims of Las Vegas Shooting"]],
			[["Dem_7"], ["Nearly 400 Trump Aides Had Access to Leaked Schedules"]],
			[["Dem_8"], ["US Budget Deficit Running 41.8 Percent above Last Year"]],
			[["Dem_9"], ["Susan Collins Raised More Money from Brett Kavanaugh Supporters than Mainers"]],
			[["Dem_10"], ["Trump's First 4 Mar-a-Lago Trips Cost Taxpayers $13.6 Million"]],
		];
		dem_headlines = shuffleArray(dem_headlines);

		rep_headlines = [
			[["Rep_1"], ["Guest Kicked Out of Disneyland for Unfurling 'Trump 2020' Banner"]],
			[["Rep_2"], ["Northam Got Nearly $2 Million in Donations from Planned Parenthood"]],
			[["Rep_3"], ["Ocasio-Cortez Retracts Erroneous Information About Green New Deal Backed by 2020 Democratic Candidates"]],
			[["Rep_4"], ["Investors Pulled Record $25 Billion from US Stock ETFs in January"]],
			[["Rep_5"], ["Migrant Jobs Project Spends 15 Million to Employ Only 120"]],
			[["Rep_6"], ["Illegal Immigration Expected to Hit Highest Level Since George W. Bush"]],
			[["Rep_7"], ["America Created 304,000 Jobs in January, Smashing Estimates"]],
			[["Rep_8"], ["76 Percent of Viewers Approve President Trump's State of the Union Speech"]],
			[["Rep_9"], ["Trump's Approval Rating among Likely Voters Soars to His Best in 23 Months at 52%"]],
			[["Rep_10"], ["Islamic State is 100 Percent Defeated"]],
		];
		rep_headlines = shuffleArray(rep_headlines);

		other_headlines = [
			[["Other_1"], ["Average Bra Sizes Rise from 34B to 36DD but Experts Split over Whether Cause Is Obesity or Fashion"]],
			[["Other_2"], ["Bird Strikes by Airplanes Tied Record in 2018, FAA Data Shows"]],
			[["Other_3"], ["For Millennials, Cancers Fueled by Obesity Are on Rise, Study Says"]],
			[["Other_4"], ["Long-haul Carrier Emirates Announces $21.4 Billion-valued Deal with Airbus"]],
			[["Other_5"], ["It's Official: 2018 Was the Fourth-warmest Year on Record"]],
			[["Other_6"], ["Queensland Floods: 500,000 Cattle Survived Years-long Drought Only to Die in the Rain"]],
			[["Other_7"], ["Turkey Orders Detention of over 1,100 People Linked to Failed Coup"]],
			[["Other_8"], ["Woody Allen Sues Amazon Studios for $68 Million over Movie Deal"]],
			[["Other_9"], ["Google to Invest $13 Billion in US Data Centers and Offices"]],
			[["Other_10"], ["Puerto Rico Wins Approval of $18 Billion Bond Restructuring"]],
		];
		other_headlines = shuffleArray(other_headlines);

		fake_headlines = [
			[["Fake_1"], ["Billionaire Founder of Corona Beer Brewery Makes Everyone in His Village a Millionaire in His Will"]],
			[["Fake_2"], ["Because of the Lack of Men, Iceland Give $5,000 Per Month to Immigrants Who Marry Icelandic Women"]],
		];

		headlines = dem_headlines.slice(0,4).concat(rep_headlines.slice(0,4));
		headlines = headlines.concat(other_headlines.slice(0,6).concat(fake_headlines));

	/*Initialize votes*/
		true_votes = [
			[["low"], [20]],
			[["low"], [25]],
			[["high"], [78]],
			[["high"], [76]],
			[["low"], [21]],
			[["low"], [24]],
			[["high"], [80]],
			[["high"], [77]],
			[["other"], [56]],
			[["other"], [43]],
			[["other"], [68]],
			[["other"], [67]],
			[["other"], [58]],
			[["other"], [69]],
			[["fake"], [12]],
			[["fake"], [10]],
		];

		/*Calculate population*/
		var population = parseInt("${e://Field/population}");

		/*Write result to engine variables*/
		for (var i = 1; i <= 16; i++) {
			Qualtrics.SurveyEngine.setEmbeddedData('headline_id_' + i.toString(), headlines[i-1][0]);
			Qualtrics.SurveyEngine.setEmbeddedData('headline_text_' + i.toString(),headlines[i-1][1]);
			Qualtrics.SurveyEngine.setEmbeddedData('true_votes_' + i.toString(),  true_votes[i-1][0]);
			true_votes_num = Math.floor(true_votes[i-1][1] * population / 100);
			Qualtrics.SurveyEngine.setEmbeddedData('true_votes_num_' + i.toString(), true_votes_num);
			Qualtrics.SurveyEngine.setEmbeddedData('true_votes_rel_' + i.toString(),100- true_votes[i-1][1]);
			Qualtrics.SurveyEngine.setEmbeddedData('false_votes_num_' + i.toString(), population - true_votes_num);
		}

});

Qualtrics.SurveyEngine.addOnUnload(function()
{
	/*Place your JavaScript here to run when the page is unloaded*/

});
